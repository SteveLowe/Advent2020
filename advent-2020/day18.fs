module advent2020.day18

open System.IO
open FParsec

type Op =
    | Addition
    | Multiplication

type Expr =
    | Expression of (Expr * Op * Expr)
    | Parenthesis of Expr
    | Number of int64

let parseMaths line =
    let ws = spaces
    let pOpen = pchar '('
    let pClose = pchar ')'
    let pAdd = pchar '+' >>% Addition
    let pMul = pchar '*' >>% Multiplication

    let pOp = ws >>. choice [ pAdd; pMul ] .>> ws
    let pNum = pint64 |>> Number

    let pExpr, pExprImpl = createParserForwardedToRef ()

    let pParens =
        between pOpen pClose pExpr |>> Parenthesis

    let pExprTrail = pOp .>>. pExpr |>> Some <|> preturn None

    let toExpression expr1 trailing =
        match trailing with
        | None -> expr1
        | Some (op, expr2) -> Expression(expr1, op, expr2)

    pExprImpl
    := pipe2 pNum pExprTrail toExpression
       <|> pipe2 pParens pExprTrail toExpression
       <|> pParens

    let result =
        match run (pExpr .>> eof) line with
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) -> failwithf "Invalid Maths: %s" errorMsg
    // above uses right recursion so the calculation is backwards
    // (fparsec cannot do left recursion)
    // so lets mirror it before returning
    // 1 + 2 * 3 + 4
    // A(1 + B(2 * C(3 + 4))
    // C(B(A(1 + 2) * 3) + 4)
    let rec mirrorExpr expr =
        match expr with
        | Number _ -> expr
        | Parenthesis e -> Parenthesis(mirrorExpr e)
        | Expression (el, op, er) ->
            let l = mirrorExpr el
            mirrorExpression l op er
    and mirrorExpression al aop b =
        // A(1 + B(2 * e)
        // B(A(1 + 2) * e)
        match b with
        | Number _ -> Expression(al, aop, b)
        | Parenthesis _ ->
            let r = mirrorExpr b
            Expression(al, aop, r)
        | Expression (bl, bop, br) ->
            let ar = mirrorExpr bl
            let a = Expression(al, aop, ar)
            mirrorExpression a bop br


    let result = result |> mirrorExpr
    result


let executeOp op a b =
    match op with
    | Addition -> a + b
    | Multiplication -> a * b

let executeExpr expr =
    let rec exec expr =
        match expr with
        | Number n -> n
        | Parenthesis e -> exec e
        | Expression (e1, op, e2) ->
            let n1 = exec e1
            let n2 = exec e2
            executeOp op n1 n2

    exec expr

let getInput file =
    File.ReadLines file
    |> Seq.map parseMaths
    |> Seq.toArray

let part1 input =
    input |> Array.map executeExpr |> Array.sum

let executeExprAddFirst (expr: Expr) =
    let rec exec expr =
        let expr = execAdd expr
        match expr with
        | Number n -> n
        | Parenthesis e -> exec e
        | Expression (l, op, r) ->
            let l = exec l
            let r = exec r
            executeOp op l r
    and execAdd b =
        match b with
        | Expression (a, bop, br) ->
            let bri = exec br
            match a with
            | Expression (z, aop, ar) ->
                let ari = (exec ar)
                match bop with
                // B(A(2 * 3) + 4) to A(2 * 7)
                // B(A(Z(2 * 3) + 4) + 5) to A(Z(2 * 3) + 9) to Z(2 * 12)
                | Addition ->
                    let a = Expression(z, aop, Number(bri + ari))
                    execAdd a
                // B(A(2 + 3) * 4) to B(5 * 4)
                | Multiplication ->
                    let a = execAdd a
                    Expression(a, bop, Number bri)
            | _ -> Expression(a, bop, Number bri)
        | _ -> b

    exec expr

let part2 input =
    input
    |> Array.map executeExprAddFirst
    |> Array.sum
