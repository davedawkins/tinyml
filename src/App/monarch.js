
//https://microsoft.github.io/monaco-editor/monarch.html
export function getMonarch() {
    return {
        defaultToken: "invalid",
        keywords: [
            "do", "in", "as", "fun", "let", "if", "then", "else", "match", "with", "try", "while", "type", "when",
            "as", "module", "rec", "interface", "end", "member", "abstract", "->", "true", "false"],
        booleans: ["true", "false"],
        tokenizer: {
            root: [
                [/@?[a-zA-Z][\w$]*/, {
                    cases: {
                        '@keywords': 'keyword',
                        '@booleans': 'boolean',
                        '@default': 'variable',
                    }
                }],
                // numbers
                [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
                [/0[xX][0-9a-fA-F]+/, 'number.hex'],
                [/-?\d+/, 'number'],
                [/".*?"/, 'string'],
                [/\/\/.*/, 'comment'],
            ],
        }
    };
}