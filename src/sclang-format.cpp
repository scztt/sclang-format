#include "SCLexer.h"
#include "SCParser.h"
#include "antlr4-runtime.h"
#include "sclang-ast-visitor.h"

#include <array>
#include <cstddef>
#include <cstring>
#include <fcntl.h>
#include <memory>
#include <ranges>
#include <regex>
#include <signal.h>
#include <string>
#include <support/Declarations.h>
#include <unistd.h>#include <iostream>

namespace {
// trim from start (in place)
static inline void ltrim(std::string& s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
                return !std::isspace(ch);
            }));
}

// trim from end (in place)
static inline void rtrim(std::string& s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(),
                         [](unsigned char ch) {
                             return !std::isspace(ch);
                         })
                .base(),
            s.end());
}

static void trimWhitespace(std::string& s)
{
    rtrim(s);
    ltrim(s);
}

static std::string trimWhitespace(std::string&& s)
{
    std::string string = s;
    trimWhitespace(s);
    return s;
}

template <typename T>
class reverse
{
private:
    T& iterable_;

public:
    explicit reverse(T& iterable)
        : iterable_{iterable}
    {
    }
    auto begin() const { return std::rbegin(iterable_); }
    auto end() const { return std::rend(iterable_); }
};

} // namespace

namespace sprklr {

enum
{
    DOCUMENT = 1000,
    EXPRESSION,
    CODEBLOCK,
    CLASS_DEFINITION,
    METHOD_DEFINITION
};

std::vector<std::string> split(const std::string& s, char delim)
{
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim))
    {
        elems.push_back(trimWhitespace(std::move(item)));
    }
    return elems;
}

struct Scope
{
    std::size_t type;
    std::size_t contextType;
    std::int64_t startLine;
    std::int64_t startIndent;
};

struct Settings
{
    static constexpr auto expressionHangingIndent{1};
    static constexpr auto indentSpaces{4};
};

using Stack = std::vector<Scope>;

bool isEndOfLine(const antlr4::Token* token)
{
    using namespace sprklr;

    switch (token->getType())
    {
        case SCLexer::NEWLINE:
        case SCLexer::COMMENT_LINE:
        case SCLexer::CARRIAGE_RETURN:
            return true;
        default:
            return false;
    }
}

bool isWhitespace(const antlr4::Token* token)
{
    using namespace sprklr;

    switch (token->getType())
    {
        case SCLexer::NEWLINE:
        case SCLexer::SPACE:
        case SCLexer::TAB:
            return true;
        default:
            return false;
    }
}

bool isScopeToken(const antlr4::Token* token)
{
    using namespace sprklr;

    switch (token->getType())
    {
        case SCLexer::PAREN_OPEN:
        case SCLexer::PAREN_CLOSE:
        case SCLexer::SQUARE_OPEN:
        case SCLexer::SQUARE_CLOSE:
        case SCLexer::CURLY_OPEN:
        case SCLexer::CURLY_CLOSE:
        case SCLexer::PIPE:
        case SCLexer::SEMICOLON:
        case SCLexer::COMMA:
        case EXPRESSION:
        case CODEBLOCK:
            return true;
        default:
            return false;
    }
}

bool isExpressionToken(const antlr4::Token* token)
{
    return !isScopeToken(token);
}

const auto& scopeBeforeLine(std::int64_t line, const Stack& stack)
{
    for (const auto& scope : reverse(stack))
    {
        if (line > scope.startLine)
        {
            return scope;
        }
    };

    return stack.front();
}

std::string Format(std::string code, int indentSpaces, bool indentWithTabs)
{
    std::string result;
    Stack scopeStack;
    bool needsNewline = false;

    scopeStack.push_back({.type = DOCUMENT, .contextType = DOCUMENT, .startLine = -1, .startIndent = -1});

    auto checkStack = [&] {
        const auto hasStack = !scopeStack.empty();
        if (!hasStack)
        {
            // raise(SIGTRAP);
        }
        return hasStack;
    };

    antlr4::ANTLRInputStream input(code);

    SCLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();

    const auto source = input.toString();

    const auto tokenList = tokens.getTokens();
    auto tokenIter = tokenList.begin();
    const auto tokenEnd = tokenList.end();
    while (tokenIter != tokenEnd)
    {
        const std::int64_t line = (*tokenIter)->getLine();
        int indent = 0;
        bool lineHasHadExpression = false; // Have we see an expression yet on this line? Note that for us, "expression"
                                           // is anything EXCEPT whitespace or a scope closure token

        const auto updateIndent = [&](const std::int64_t offset) {
            // LOGIC:
            // We continue to update the current line indent so long as it has not yet had any
            // expressions. Once there's an expression (anything other than ws/scope closure),
            // the indent is fixed for that line. This allows us to use the post-scope-closure indent
            // for the last line of things like this:
            // f = {
            //     |x|
            // }  <--- indent takes the initial scope closure into account.
            if (!lineHasHadExpression)
            {
                const auto& scope = scopeBeforeLine(line, scopeStack);

                if (scope.type == CODEBLOCK)
                {
                    // LOGIC: Chilren of code blocks aren't indented
                    indent = scope.startIndent;
                }
                else if (scope.type == EXPRESSION &&
                         (scope.contextType == SCLexer::CURLY_OPEN || scope.contextType == SCLexer::SQUARE_OPEN ||
                          scope.contextType == SCLexer::PAREN_OPEN || scope.contextType == SCLexer::PIPE ||
                          scope.contextType == CODEBLOCK))
                {
                    // LOGIC: Chilren of expressions aren't indented EXCEPT when we choose to indent them
                    // See EXPRESSION INDENT LOGIC
                    indent = scope.startIndent;
                }
                else
                {
                    indent = offset + scope.startIndent;
                }
            }
        };

        updateIndent(1);
        std::int64_t startPos = (*tokenIter)->getStartIndex();
        std::int64_t endPos = startPos;

        //////////////////////////////////////////////////////////////////////////////////////////////////
        // START LINE PROCESSING
        while (tokenIter != tokenEnd)
        {
            const auto* token = *tokenIter++;

            const std::int64_t character = token->getCharPositionInLine();

            const auto startExpression = [&] {
                // LOGIC:
                // Do not start an expression if we're already in one
                if (checkStack() && scopeStack.back().type != EXPRESSION)
                {
                    scopeStack.push_back({.type = EXPRESSION,
                                          .contextType = scopeStack.back().type,
                                          .startLine = line,
                                          .startIndent = indent + Settings::expressionHangingIndent});
                }
            };

            using StackQueryFunc = std::function<bool(const Scope&)>;
            const auto stackQuery = [&](auto... funcs) {
                auto funcArray = std::array<StackQueryFunc, sizeof...(funcs)>{funcs...};
                if (funcArray.size() > scopeStack.size())
                {
                    return false;
                }

                auto index = scopeStack.size() - 1;

                for (const auto& func : funcArray)
                {
                    if (!func(scopeStack[index]))
                    {
                        return false;
                    }

                    index--;
                }

                return true;
            };

            const auto endExpression = [&] {
                if (checkStack() && scopeStack.back().type == EXPRESSION)
                {
                    scopeStack.pop_back();
                }
            };

            const auto startScope = [&] {
                // LOGIC:
                // Starting a new scope counts as an expression for our purposes.
                lineHasHadExpression = true;

                const auto isCodeBlock =
                    (token->getType() == SCLexer::PAREN_OPEN && (checkStack() && scopeStack.back().type == DOCUMENT));

                const auto isClassDefinition =
                    (token->getType() == SCLexer::CLASSNAME && (checkStack() && scopeStack.back().type == DOCUMENT));

                const auto isMethodDefinition =
                    (token->getType() == SCLexer::NAME && stackQuery(
                                                              [](auto s) {
                                                                  return s.type == SCLexer::CURLY_OPEN;
                                                              },
                                                              [](auto s) {
                                                                  return s.contextType == CLASS_DEFINITION;
                                                              }));

                const auto computedType = (isCodeBlock        ? CODEBLOCK :
                                           isClassDefinition  ? CLASS_DEFINITION :
                                           isMethodDefinition ? METHOD_DEFINITION :
                                                                token->getType());

                scopeStack.push_back({.type = computedType,
                                      .contextType = scopeStack.back().type,
                                      .startLine = static_cast<int64_t>(line),
                                      .startIndent = indent});
            };

            const auto endScope = [&] {
                // LOGIC:
                // Ending a scope also ends whatever expression was happening (e.g. you don't need a ; or , on the last
                // line of a function or array).
                // updateIndent(0) allows us to dedent back to the originating indentation level if we're on a line with
                // only scope closures
                endExpression();
                updateIndent(0);

                if (!checkStack())
                    return;

                scopeStack.pop_back();

                if (checkStack())
                {
                    // LOGIC: Class and method definitions are bound by {}, but have no explicit end token. So, we can
                    // pop these whenever the scope inside of them ends.
                    if (stackQuery([](auto s) {
                            return s.type == EXPRESSION && s.contextType == CLASS_DEFINITION;
                        }))
                    {
                        scopeStack.pop_back();
                    }

                    if (stackQuery([](auto s) {
                            return s.type == CLASS_DEFINITION || s.type == METHOD_DEFINITION;
                            ;
                        }))
                    {
                        scopeStack.pop_back();
                    }
                }
            };

            if (isEndOfLine(token) || (tokenIter == tokenEnd))
            {
                endPos = token->getStopIndex() - 1;
                break;
            }

            if (isWhitespace(token))
            {
                continue;
            }

            if (isExpressionToken(token))
            {
                lineHasHadExpression = true;
            }

            switch (token->getType())
            {
                case SCLexer::PAREN_OPEN:
                case SCLexer::SQUARE_OPEN:
                case SCLexer::CURLY_OPEN:
                    startScope();
                    break;

                case SCLexer::PAREN_CLOSE:
                case SCLexer::SQUARE_CLOSE:
                case SCLexer::CURLY_CLOSE:
                    endScope();
                    break;

                case SCLexer::PIPE:
                    if (checkStack() && scopeStack.back().type == SCLexer::CURLY_OPEN)
                    {
                        startScope();
                    }
                    else if (checkStack() && scopeStack.back().contextType == SCLexer::PIPE)
                    {
                        endScope();
                    }
                    break;

                case SCLexer::SEMICOLON:
                {
                    if (checkStack())
                    {
                        const auto& scope = scopeStack.back();
                        if (scope.type == EXPRESSION)
                        {
                            switch (scope.contextType)
                            {
                                case SCLexer::CURLY_OPEN:
                                case SCLexer::PAREN_OPEN:
                                case CODEBLOCK:
                                case DOCUMENT:
                                    endExpression();
                            }
                        }
                    }
                    break;
                }

                case SCLexer::COMMA:
                {
                    if (checkStack())
                    {
                        const auto& scope = scopeStack.back();
                        if (scope.type == EXPRESSION)
                        {
                            switch (scope.contextType)
                            {
                                case SCLexer::SQUARE_OPEN:
                                case SCLexer::PAREN_OPEN:
                                case SCLexer::PIPE:
                                    endExpression();
                            }
                        }
                    }

                    break;
                }

                case SCLexer::CLASSNAME:
                    if (checkStack())
                    {
                        if (scopeStack.back().type == DOCUMENT)
                        {
                            // Class definition.
                            startScope();
                            // break;
                        }
                    }
                    startExpression();
                    break;

                case SCLexer::PLUS:
                    if (checkStack())
                    {
                        if (scopeStack.back().type == DOCUMENT)
                        {
                            // LOGIC: We sloppily assume that this is a +Class extension
                            // and simply ignore this token.
                            break;
                        }
                    }
                    startExpression();
                    break;

                case SCLexer::NAME:
                    if (checkStack())
                    {
                        // if (scopeStack.back().type == SCLexer::CURLY_OPEN &&
                        //     scopeStack.back().contextType == CLASS_DEFINITION)
                        if (stackQuery(
                                [](auto& s) {
                                    return s.type == SCLexer::CURLY_OPEN;
                                },
                                [](auto& s) {
                                    return s.contextType == CLASS_DEFINITION;
                                }))
                        {
                            // Method definition.
                            startScope();
                            break;
                        }
                    }
                    startExpression();
                    break;

                case SCLexer::COMMENT_LINE:
                case SCLexer::COMMENT_BLOCK:
                    break;

                default:
                    startExpression();
                    break;
            }
        }
        // END LINE PROCESSING
        //////////////////////////////////////////////////////////////////////////////////////////////////

        // auto lineString = source.substr(startPos, endPos - startPos);
        antlr4::misc::Interval lineInterval{(size_t)startPos, (size_t)endPos};
        auto lineString = input.getText(lineInterval);
        ltrim(lineString);
        // std::cout << ":" << lineString << ":\n";

        if (needsNewline)
        {
            result += "\n";
        }
        needsNewline = true;

        // if (lineString.size() > 0)
        {
            auto indentString = indentWithTabs ? std::string(std::max(0, indent), '\t') :
                                                 std::string(std::max(0, indent) * indentSpaces, ' ');
            // std::cout << "\t" << line << "\t|" << indentString + lineString + "\n";
            result += indentString + lineString;
        }

        // EXPRESSION INDENT LOGIC:
        // Expressions UPDATE their indentation based on the indentation of the last line used in that expression.
        // So, if the last line of an expression ended with indent=0, the next line will start on 0.
        // If the last line ended with indent=2, the next line will start on 2.
        // This only applies to expressions of three or more lines - with two lines, we just follow the normal indent
        // rules.
        // EXAMPLES:
        // foo
        //   .bar; <-- indented
        if (checkStack() && (scopeStack.back().type == EXPRESSION) && (line > (scopeStack.back().startLine + 1)))
        {
            scopeStack.back().startIndent = indent;
        };
    }

    return result;
}
} // namespace sprklr

void ast_printer()
{
    bool shouldWait = false;

    while (shouldWait)
    {
    }

    uint spinCount = 0;
    while (!std::cin.bad())
    {
        std::string text;
        std::getline(std::cin, text, (char)0);

        if (text.size() > 0)
        {
            sclang_ast_visitor::print_ast(text, std::cout);

            std::cout << (char)0;
            std::cout.flush();
        }
        else
        {
            spinCount++;
            if (spinCount > 100)
            {
                break;
            }
        }

        // if (!wait) {
        //     break;
        // }
    }
}

int main(int argc, const char* argv[])
{
    using namespace sprklr;

    auto indentSpaces = 4;
    auto indentWithTabs = false;

    bool shouldWait = false;
    bool astPrinter = false;

    for (int i = 0; i < argc; ++i)
    {
        auto curArg = argv[i];

        if (strcmp(curArg, "-t") == 0)
        {
            indentWithTabs = true;
        }
        else if (strstr(curArg, "-i"))
        {
            if (i + 1 < argc)
            {
                indentSpaces = std::stoi(argv[i + 1]);
            }
        }
        else if (strstr(curArg, "-w"))
        {
            shouldWait = true;
        }
        else if (strstr(curArg, "-p"))
        {
            astPrinter = true;
        }
    }

    // while (wait)
    // {
    // }

    if (astPrinter)
    {
        ast_printer();
        return 0;
    };

    // int flags = fcntl(STDOUT_FILENO, F_GETFL, 0);
    // fcntl(STDOUT_FILENO, F_SETFL, flags | O_NONBLOCK);

    uint spinCount = 0;
    while (!std::cin.bad() && !std::cin.eof())
    {
        std::string text;
        std::getline(std::cin, text, (char)0);

        if (text.size() > 0)
        {
            constexpr char endChar = 4; // ctrl-d
            if (text == std::string{endChar})
            {
                return 0;
            }

            if (text.back() != '\n')
            {
                text += '\n';
            }

            auto result = sprklr::Format(text, indentSpaces, indentWithTabs);
            std::cout << result << (char)0;
            std::cout.flush();
        }
        else
        {
            spinCount++;
            if (spinCount > 100)
            {
                break;
            }
        }

        if (!shouldWait)
            break;
    }

    auto bad = std::cin.bad();
    auto eof = std::cin.eof();

    return 0;
}
