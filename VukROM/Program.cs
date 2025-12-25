using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

namespace x
{
    class Y
    {
        // Wait's
        static void WaitMS(int ms) { System.Threading.Thread.Sleep(ms); }
        static void WaitS(int s) { System.Threading.Thread.Sleep(s * 1000); }

        // Variables storage
        static readonly Dictionary<string, object> Vars = new Dictionary<string, object>(StringComparer.Ordinal);

        // Entry
        static void Main(string[] args)
        {
            Console.OutputEncoding = Encoding.UTF8;
            Console.WriteLine("Vuk ROM Loading..");

            string romPath = Path.Combine(AppContext.BaseDirectory, "kernel.rom");
            if (!File.Exists(romPath))
            {
                Console.WriteLine("Failed: No Kernel");
                Console.ReadKey(true);
                return;
            }
            Console.WriteLine("Success.");
            Console.Clear();

            string[] lines;
            try { lines = File.ReadAllLines(romPath); }
            catch (Exception ex)
            {
                Console.WriteLine("Failed: ROM read error");
                Console.WriteLine(ex.Message);
                Console.ReadKey(true);
                return;
            }

            // Validate tag (#VUKROMFILE must be the first non-skippable line)
            int first = 0;
            while (first < lines.Length && IsSkippable(lines[first])) first++;
            if (first >= lines.Length || !lines[first].Trim().Equals("#VUKROMFILE", StringComparison.Ordinal))
            {
                Console.WriteLine("Failed: Invalid ROM tag");
                Console.ReadKey(true);
                return;
            }

            // Parse blocks and commands
            Dictionary<string, List<Command>> blocks;
            try { blocks = ParseBlocks(lines); }
            catch (RomExecException rex)
            {
                Console.WriteLine($"Failed: {rex.Message}");
                Console.ReadKey(true);
                return;
            }

            if (!blocks.TryGetValue("main", out var mainBlock))
            {
                Console.WriteLine("Failed: No 'main' block");
                Console.ReadKey(true);
                return;
            }

            // Execute main
            try { ExecuteBlock(mainBlock); }
            catch (RomExecException rex)
            {
                Console.WriteLine($"Failed: {rex.Message}");
            }
            catch (Exception ex)
            {
                Console.WriteLine("Failed: Unexpected error");
                Console.WriteLine(ex.Message);
            }

            Console.ReadKey(true);
        }

        // --- Parsing ---

        static bool IsSkippable(string line)
        {
            if (line == null) return true;
            var t = line.Trim();
            return t.Length == 0 || t.StartsWith("//");
        }

        static string StripComment(string line)
        {
            if (line == null) return "";
            int idx = line.IndexOf("//");
            return idx >= 0 ? line.Substring(0, idx) : line;
        }

        static bool IsLabelLine(string line, out string label)
        {
            label = string.Empty;
            var m = Regex.Match(line, @"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*$");
            if (m.Success) { label = m.Groups[1].Value; return true; }
            return false;
        }

        static Dictionary<string, List<Command>> ParseBlocks(string[] rawLines)
        {
            var blocks = new Dictionary<string, List<Command>>(StringComparer.Ordinal);
            List<Command> current = new List<Command>();
            string currentLabel = string.Empty;

            // Preprocess: strip comments, keep original indexing for error context
            var lines = new List<(string text, int lineNo)>();
            for (int i = 0; i < rawLines.Length; i++)
            {
                var clean = StripComment(rawLines[i]).TrimEnd();
                lines.Add((clean, i + 1));
            }

            for (int i = 0; i < lines.Count; i++)
            {
                var (text, lineNo) = lines[i];
                var trimmed = text.Trim();
                if (trimmed.Length == 0) continue;
                if (trimmed.Equals("#VUKROMFILE", StringComparison.Ordinal)) continue;

                if (IsLabelLine(trimmed, out string label))
                {
                    currentLabel = label;
                    current = new List<Command>();
                    blocks[currentLabel] = current;
                    continue;
                }

                if (current == null)
                    throw new RomExecException($"Command outside of any block at line {lineNo}");

                // Parse command at this line (including possible nested blocks for if/while)
                var (cmd, consumedLines) = ParseCommand(lines, i);
                current.Add(cmd);
                i = consumedLines - 1; // advance
            }

            return blocks;
        }

        static (Command cmd, int consumedUntilExclusive) ParseCommand(List<(string text, int lineNo)> lines, int startIdx)
        {
            var (text, lineNo) = lines[startIdx];
            var trimmed = text.Trim();
            if (trimmed.Length == 0) return (new NopCommand(), startIdx + 1);

            // var assignment: var name = expr
            if (trimmed.StartsWith("var "))
            {
                var rest = trimmed.Substring(4);
                var parts = rest.Split(new[] { '=' }, 2);
                if (parts.Length != 2) throw new RomExecException($"Invalid var syntax at line {lineNo}: {trimmed}");
                string name = parts[0].Trim();
                string expr = parts[1].Trim();
                return (new VarAssignCommand(name, expr, lineNo), startIdx + 1);
            }

            // if (expr) { ... }
            var ifMatch = Regex.Match(trimmed, @"^\s*if\s*\((.+)\)\s*\{\s*$");
            if (ifMatch.Success)
            {
                string condExpr = ifMatch.Groups[1].Value.Trim();
                var (body, next) = ParseBracedBlock(lines, startIdx + 1);
                return (new IfCommand(condExpr, body, lineNo), next);
            }

            // while (expr) { ... }
            var whileMatch = Regex.Match(trimmed, @"^\s*while\s*\((.+)\)\s*\{\s*$");
            if (whileMatch.Success)
            {
                string condExpr = whileMatch.Groups[1].Value.Trim();
                var (body, next) = ParseBracedBlock(lines, startIdx + 1);
                return (new WhileCommand(condExpr, body, lineNo), next);
            }

            // Single-line function-like commands: name(args)
            var callMatch = Regex.Match(trimmed, @"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*$");
            if (callMatch.Success)
            {
                string name = callMatch.Groups[1].Value;
                string args = callMatch.Groups[2].Value.Trim();
                switch (name)
                {
                    case "write": return (new WriteCommand(args, lineNo), startIdx + 1);
                    case "wait":
                    case "waits": return (new WaitSCommand(args, lineNo), startIdx + 1);
                    case "waitms": return (new WaitMSCommand(args, lineNo), startIdx + 1);
                    case "read": return (new ReadCommand(args, lineNo), startIdx + 1);
                    case "load": return (new LoadCommand(args, lineNo), startIdx + 1);
                    case "save": return (new SaveCommand(args, lineNo), startIdx + 1);
                    case "run": return (new RunCommand(args, lineNo), startIdx + 1);
                    default: throw new RomExecException($"Unknown command '{name}' at line {lineNo}");
                }
            }

            // Closing brace alone (handled by ParseBracedBlock)
            if (trimmed == "}") throw new RomExecException($"Unexpected closing brace at line {lineNo}");

            throw new RomExecException($"Invalid command syntax at line {lineNo}: {trimmed}");
        }

        static (List<Command> body, int nextIdx) ParseBracedBlock(List<(string text, int lineNo)> lines, int idx)
        {
            var body = new List<Command>();
            for (int i = idx; i < lines.Count; i++)
            {
                var (text, lineNo) = lines[i];
                var trimmed = text.Trim();

                if (trimmed.Length == 0) continue;

                if (trimmed == "}")
                {
                    return (body, i + 1); // consume up to closing brace
                }

                var (cmd, consumed) = ParseCommand(lines, i);
                body.Add(cmd);
                i = consumed - 1;
            }
            throw new RomExecException("Missing closing brace '}' for block");
        }

        // --- Commands ---

        abstract class Command { public readonly int LineNo; protected Command(int ln) { LineNo = ln; } public abstract void Exec(); }
        class NopCommand : Command { public NopCommand() : base(0) { } public override void Exec() { } }

        class VarAssignCommand : Command
        {
            readonly string Name;
            readonly string Expr;
            public VarAssignCommand(string name, string expr, int ln) : base(ln) { Name = name; Expr = expr; }
            public override void Exec()
            {
                object val = EvalExpression(Expr);
                Vars[Name] = val;
            }
        }

        class WriteCommand : Command
        {
            readonly string ArgExpr;
            public WriteCommand(string expr, int ln) : base(ln) { ArgExpr = expr; }
            public override void Exec()
            {
                var val = EvalExpression(ArgExpr);
                Console.WriteLine(val?.ToString() ?? "");
            }
        }

        class WaitSCommand : Command
        {
            readonly string ArgExpr;
            public WaitSCommand(string expr, int ln) : base(ln) { ArgExpr = expr; }
            public override void Exec()
            {
                var val = EvalExpression(ArgExpr);
                if (val is int iv) WaitS(iv);
                else throw new RomExecException($"wait expects integer, got {FmtVal(val)}");
            }
        }

        class WaitMSCommand : Command
        {
            readonly string ArgExpr;
            public WaitMSCommand(string expr, int ln) : base(ln) { ArgExpr = expr; }
            public override void Exec()
            {
                var val = EvalExpression(ArgExpr);
                if (val is int iv) WaitMS(iv);
                else throw new RomExecException($"waitms expects integer, got {FmtVal(val)}");
            }
        }

        class ReadCommand : Command
        {
            readonly string TargetName;
            public ReadCommand(string args, int ln) : base(ln)
            {
                TargetName = args.Trim();
                if (!Regex.IsMatch(TargetName, @"^[A-Za-z_][A-Za-z0-9_]*$"))
                    throw new RomExecException($"Invalid read target at line {ln}: {TargetName}");
            }
            public override void Exec()
            {
                var input = Console.ReadLine() ?? "";
                Vars[TargetName] = input;
            }
        }

        class LoadCommand : Command
        {
            readonly string VarName;
            readonly string FileExpr; // should be string literal or expression yielding string
            public LoadCommand(string args, int ln) : base(ln)
            {
                var parts = SplitArgs(args, 2, ln);
                VarName = parts[0].Trim();
                FileExpr = parts[1].Trim();
                if (!Regex.IsMatch(VarName, @"^[A-Za-z_][A-Za-z0-9_]*$"))
                    throw new RomExecException($"Invalid variable name in load at line {ln}: {VarName}");
            }

            public override void Exec()
            {
                var fval = EvalExpression(FileExpr);
                if (fval is string path)
                {
                    try { Vars[VarName] = File.ReadAllText(path); }
                    catch (Exception ex) { throw new RomExecException($"load failed: {ex.Message}"); }
                }
                else throw new RomExecException($"load expects string path, got {FmtVal(fval)}");
            }
        }

        class SaveCommand : Command
        {
            readonly string FileExpr; // string path
            readonly string DataExpr; // any expr -> string written via ToString
            public SaveCommand(string args, int ln) : base(ln)
            {
                var parts = SplitArgs(args, 2, ln);
                FileExpr = parts[0].Trim();
                DataExpr = parts[1].Trim();
            }
            public override void Exec()
            {
                var fval = EvalExpression(FileExpr);
                var dval = EvalExpression(DataExpr);
                if (fval is string path)
                {
                    try { File.WriteAllText(path, dval?.ToString() ?? ""); }
                    catch (Exception ex) { throw new RomExecException($"save failed: {ex.Message}"); }
                }
                else throw new RomExecException($"save expects string path, got {FmtVal(fval)}");
            }
        }

        class IfCommand : Command
        {
            readonly string CondExpr;
            readonly List<Command> Body;
            public IfCommand(string cond, List<Command> body, int ln) : base(ln) { CondExpr = cond; Body = body; }
            public override void Exec()
            {
                var cond = EvalBool(CondExpr);
                if (cond) ExecuteBlock(Body);
            }
        }

        class WhileCommand : Command
        {
            readonly string CondExpr;
            readonly List<Command> Body;
            public WhileCommand(string cond, List<Command> body, int ln) : base(ln) { CondExpr = cond; Body = body; }
            public override void Exec()
            {
                int guard = 0; // simple runaway loop guard (optional)
                while (EvalBool(CondExpr))
                {
                    ExecuteBlock(Body);
                    guard++;
                    if (guard > 10_000_000) throw new RomExecException("while guard triggered");
                }
            }
        }

        class PowerCommand : Command
        {
            readonly string Mode;
            public PowerCommand(string arg, int ln) : base(ln)
            {
                Mode = arg.Trim().ToLowerInvariant();
                if (Mode != "shutdown" && Mode != "sleep")
                    throw new RomExecException($"Invalid power mode at line {ln}: {arg}");
            }

            public override void Exec()
            {
                switch (Mode)
                {
                    case "shutdown":
                        Environment.Exit(0);
                        break;
                    // Using Switch/Case to easily add more power features when we implement them
                }
            }
        }

        class RunCommand : Command
        {
            readonly string TargetExpr;
            public RunCommand(string args, int ln) : base(ln) { TargetExpr = args.Trim(); }

            public override void Exec()
            {
                var val = EvalExpression(TargetExpr);
                if (val is not string path)
                    throw new RomExecException($"run expects string path, got {FmtVal(val)}");

                string fullPath = Path.GetFullPath(path);

                if (fullPath.EndsWith(".rom", StringComparison.OrdinalIgnoreCase))
                {
                    // Load and execute another ROM file
                    if (!File.Exists(fullPath))
                        throw new RomExecException($"ROM file not found: {fullPath}");

                    string[] lines = File.ReadAllLines(fullPath);
                    var blocks = ParseBlocks(lines);
                    if (!blocks.TryGetValue("main", out var mainBlock))
                        throw new RomExecException($"ROM file has no 'main' block: {fullPath}");

                    ExecuteBlock(mainBlock);
                }
                else if (fullPath.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                {
                    if (!File.Exists(fullPath))
                        throw new RomExecException($"Executable not found: {fullPath}");

                    try
                    {
                        var proc = new System.Diagnostics.Process();
                        proc.StartInfo.FileName = fullPath;
                        proc.StartInfo.UseShellExecute = true;
                        proc.Start();
                        proc.WaitForExit();
                    }
                    catch (Exception ex)
                    {
                        throw new RomExecException($"Failed to run exe: {ex.Message}");
                    }
                }
                else
                {
                    throw new RomExecException($"Unsupported run target: {fullPath}");
                }
            }
        }



        static void ExecuteBlock(List<Command> commands)
        {
            foreach (var c in commands) c.Exec();
        }

        // --- Expression evaluation ---

        // Grammar:
        // Expr := Compare
        // Compare := AddSub ( (== | != | < | > | <= | >=) AddSub )?
        // AddSub := Term ( ('+' | '-') Term )*
        // Term := String | Int | Ident | '(' Expr ')'
        //
        // Types: int, string, bool (only for condition evaluation). '+' works for int+int and string+string.

        static object EvalExpression(string expr)
        {
            var tk = new Tokenizer(expr);
            var val = ParseCompare(tk);
            tk.ExpectEnd();
            return val;
        }

        static bool EvalBool(string expr)
        {
            var v = EvalExpression(expr);
            if (v is bool b) return b;
            if (v is int i) return i != 0;
            if (v is string s) return s.Length != 0;
            throw new RomExecException($"Expected boolean expression, got {FmtVal(v)}");
        }

        static object ParseCompare(Tokenizer tk)
        {
            var left = ParseAddSub(tk);
            if (tk.Match("==")) return CompareOp(left, ParseAddSub(tk), "==");
            if (tk.Match("!=")) return CompareOp(left, ParseAddSub(tk), "!=");
            if (tk.Match("<=")) return CompareOp(left, ParseAddSub(tk), "<=");
            if (tk.Match(">=")) return CompareOp(left, ParseAddSub(tk), ">=");
            if (tk.Match("<")) return CompareOp(left, ParseAddSub(tk), "<");
            if (tk.Match(">")) return CompareOp(left, ParseAddSub(tk), ">");
            return left;
        }

        static object ParseAddSub(Tokenizer tk)
        {
        var acc = ParseTerm(tk);
        while (true)
        {
            if (tk.Match("+"))
            {
                var rhs = ParseTerm(tk);
                if (acc is int li && rhs is int ri)
                    acc = li + ri;
                else if (acc is string ls && rhs is string rs)
                    acc = ls + rs;
                else if (acc is string ls2 && rhs is int ri2)
                    acc = ls2 + ri2.ToString();
                else if (acc is int li2 && rhs is string rs2)
                    acc = li2.ToString() + rs2;
                else
                    throw new RomExecException($"Invalid '+' operands: {FmtVal(acc)} + {FmtVal(rhs)}");
            }
            else if (tk.Match("-"))
            {
                var rhs = ParseTerm(tk);
                if (acc is int li && rhs is int ri)
                    acc = li - ri;
                else
                    throw new RomExecException($"Invalid '-' operands: {FmtVal(acc)} - {FmtVal(rhs)}");
            }
            else break;
        }
        return acc;
        }


        static object ParseTerm(Tokenizer tk)
        {
            if (tk.Match("("))
            {
                var inner = ParseCompare(tk);
                tk.Expect(")");
                return inner;
            }

            if (tk.PeekType() == TokenType.String) return tk.ConsumeString();
            if (tk.PeekType() == TokenType.Int) return tk.ConsumeInt();
            if (tk.PeekType() == TokenType.Ident)
            {
                var name = tk.ConsumeIdent();
                if (Vars.TryGetValue(name, out var val)) return val;
                // undefined variable: treat as error
                throw new RomExecException($"Unknown variable '{name}'");
            }

            throw new RomExecException($"Unexpected token at pos {tk.Pos}");
        }

        static bool CompareOp(object left, object right, string op)
        {
            switch (op)
            {
                case "==": return Stringify(left) == Stringify(right);
                case "!=": return Stringify(left) != Stringify(right);
                case "<":
                    if (left is int li && right is int ri) return li < ri;
                    return string.Compare(Stringify(left), Stringify(right), StringComparison.Ordinal) < 0;
                case ">":
                    if (left is int lg && right is int rg) return lg > rg;
                    return string.Compare(Stringify(left), Stringify(right), StringComparison.Ordinal) > 0;
                case "<=":
                    if (left is int li2 && right is int ri2) return li2 <= ri2;
                    return string.Compare(Stringify(left), Stringify(right), StringComparison.Ordinal) <= 0;
                case ">=":
                    if (left is int lg2 && right is int rg2) return lg2 >= rg2;
                    return string.Compare(Stringify(left), Stringify(right), StringComparison.Ordinal) >= 0;
                default: throw new RomExecException($"Unknown compare op {op}");
            }
        }

        static string Stringify(object v) => v?.ToString() ?? "";

        static string[] SplitArgs(string args, int expected, int lineNo)
        {
            // Split by commas respecting quotes and parentheses minimally
            var res = new List<string>();
            var sb = new StringBuilder();
            int depth = 0;
            bool inStr = false;

            for (int i = 0; i < args.Length; i++)
            {
                char c = args[i];
                if (inStr)
                {
                    sb.Append(c);
                    if (c == '\\' && i + 1 < args.Length) { sb.Append(args[++i]); continue; }
                    if (c == '"') inStr = false;
                    continue;
                }

                if (c == '"') { inStr = true; sb.Append(c); continue; }
                if (c == '(') { depth++; sb.Append(c); continue; }
                if (c == ')') { depth--; sb.Append(c); continue; }

                if (c == ',' && depth == 0)
                {
                    res.Add(sb.ToString().Trim());
                    sb.Clear();
                }
                else sb.Append(c);
            }
            if (sb.Length > 0) res.Add(sb.ToString().Trim());

            if (res.Count != expected)
                throw new RomExecException($"Expected {expected} argument(s), got {res.Count} at line {lineNo}: ({args})");
            return res.ToArray();
        }

        static string FmtVal(object v)
        {
            if (v == null) return "null";
            return $"{v} ({v.GetType().Name})";
        }

        // --- Tokenizer ---

        enum TokenType { EOF, Ident, Int, String, Symbol }

        class Tokenizer
        {
            readonly string S;
            public int Pos { get; private set; }
            public Tokenizer(string s) { S = s ?? ""; Pos = 0; SkipWS(); }

            void SkipWS()
            {
                while (Pos < S.Length && char.IsWhiteSpace(S[Pos])) Pos++;
            }

            public TokenType PeekType()
            {
                if (Pos >= S.Length) return TokenType.EOF;
                char c = S[Pos];
                if (c == '"') return TokenType.String;
                if (char.IsDigit(c) || (c == '-' && Pos + 1 < S.Length && char.IsDigit(S[Pos + 1]))) return TokenType.Int;
                if (char.IsLetter(c) || c == '_') return TokenType.Ident;
                return TokenType.Symbol;
            }

            public bool Match(string sym)
            {
                SkipWS();
                if (S.Substring(Pos).StartsWith(sym))
                {
                    Pos += sym.Length;
                    SkipWS();
                    return true;
                }
                return false;
            }

            public void Expect(string sym)
            {
                if (!Match(sym)) throw new RomExecException($"Expected '{sym}' at pos {Pos}");
            }

            public void ExpectEnd()
            {
                SkipWS();
                if (Pos != S.Length) throw new RomExecException($"Unexpected trailing input at pos {Pos}");
            }

            public string ConsumeIdent()
            {
                SkipWS();
                int start = Pos;
                if (!(char.IsLetter(S[Pos]) || S[Pos] == '_')) throw new RomExecException($"Expected identifier at pos {Pos}");
                Pos++;
                while (Pos < S.Length && (char.IsLetterOrDigit(S[Pos]) || S[Pos] == '_')) Pos++;
                SkipWS();
                return S.Substring(start, Pos - start);
            }

            public int ConsumeInt()
            {
                SkipWS();
                int start = Pos;
                if (S[Pos] == '-') Pos++;
                if (Pos >= S.Length || !char.IsDigit(S[Pos])) throw new RomExecException($"Expected integer at pos {Pos}");
                while (Pos < S.Length && char.IsDigit(S[Pos])) Pos++;
                SkipWS();
                var token = S.Substring(start, Pos - start);
                if (!int.TryParse(token, out int v)) throw new RomExecException($"Invalid integer literal: {token}");
                return v;
            }

            public string ConsumeString()
            {
                SkipWS();
                if (S[Pos] != '"') throw new RomExecException($"Expected string at pos {Pos}");
                Pos++; // skip opening "
                var sb = new StringBuilder();
                while (Pos < S.Length)
                {
                    char c = S[Pos++];
                    if (c == '\\')
                    {
                        if (Pos >= S.Length) throw new RomExecException("Invalid escape at end of string");
                        char n = S[Pos++];
                        switch (n)
                        {
                            case '\\': sb.Append('\\'); break;
                            case '"': sb.Append('"'); break;
                            case 'n': sb.Append('\n'); break;
                            case 'r': sb.Append('\r'); break;
                            case 't': sb.Append('\t'); break;

                            case 'u':
                                // Ensure we have at least 4 hex digits available
                                if (Pos + 4 > S.Length) throw new RomExecException("Invalid \\u escape");
                                string hex = S.Substring(Pos, 4); // grab exactly 4 chars
                                if (!int.TryParse(hex, System.Globalization.NumberStyles.HexNumber, null, out int code))
                                    throw new RomExecException($"Invalid \\u escape: {hex}");
                                sb.Append((char)code);
                                Pos += 4; // advance past those 4 hex digits
                                break;

                            default: throw new RomExecException($"Unknown escape \\{n}");
                        }

                        continue;
                    }
                    if (c == '"') { SkipWS(); return sb.ToString(); }
                    sb.Append(c);
                }
                throw new RomExecException("Unterminated string literal");
            }
        }

        // --- Errors ---
        class RomExecException : Exception { public RomExecException(string msg) : base(msg) { } }
    }
}