% Identifying change patterns in software history
% Jason Dagit
% Galois, Inc


# Motivation

Tools to detect changes exist.

\vspace{2em}

For example, traditional line-based diff:

* Pro: diff is very general and programming language agnostic

* Con: diff is not structurally aware:

```
       if( foo ){           if( foo )
         bar;               {
       }                      bar;
                            }
```

\vspace{2em}

We need tools for interpreting changes.


# Motivation

Common looping pattern with loop counter initialized to zero:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

\vspace{1em}

We also want to see how source code *changes*.

# Example from Clojure: Related edits

\Small Our tool found these related edits:

`PersistentArrayMap.java`

```
 public Object kvreduce(IFn f, Object init){
     for(int i=0;i < array.length;i+=2){
         init = f.invoke(init, array[i], array[i+1]);
-           if(RT.isReduced(init))
-                   return ((IDeref)init).deref();
         }
     return init;
 }
```

\hrule

`PersistentHashMap.java`

```
 public Object kvreduce(IFn f, Object init){
-    for(INode node : array){
-        if(node != null){
+    for(INode node : array)
+        {
+        if(node != null)
             init = node.kvreduce(f,init);
-                if(RT.isReduced(init))
-                        return ((IDeref)init).deref();
-               }
-           }
+        }
     return init;
 }
```

# Approach

  **Key Idea:** We can find *structural patterns* by generalizing *sufficiently
similar* difference trees.

  * Difference trees computed using structural diff of AST

  * Similarity is measured using a tree edit distance score

  * Generalization is accomplished through *antiunification*

# Workflow

\begin{center}

  \includegraphics[height=0.8\textheight]{figures/workflow.pdf}

\end{center}

# ATerms

```
i++;
```

\hrule

\begin{verbatim}
AAppl "ExpStmt"
  [AAppl "PostIncrement"
    [AAppl "ExpName"
      [AAppl "Name"
        [AList
          [AAppl "Ident" [AAppl "\"i\"" []]]]]]]
\end{verbatim}

\vspace{1em}

Generic tree structure---programming language agnostic.

\vspace{1em}

Easy to modify parsers to generate ATerms.

# Structural diff

$$
treediff\left(\;\raisebox{1.5em}{\Tree[.A [.B ] [.C ]]}\quad,\quad\raisebox{1.5em}{\Tree[.A [.B [.D ] ] [.F ]]}\;\right) = \raisebox{1.5em}{\Tree[.A [.B [.lefthole(D) ] ] [.mismatch(C,F) ]]}
$$

Keep just the differences with a bit of context:
$$
t_a = \raisebox{1em}{\Tree[.A [.mismatch(C,F) ]]} \hspace{2em} t_b = \raisebox{1em}{\Tree[.B [.lefthole(D) ] ] }
$$

Output also gives us an edit distance.

# Workflow

\begin{center}

  \includegraphics[height=0.8\textheight]{figures/workflow.pdf}

\end{center}

# Similarity grouping

We define the similarity score by:

$$\Delta(t_a, t_b) := \frac{min(d(t_a, t_b),d(t_b, t_a))}{max(|t_a|,|t_b|)}$$
where $d$ is the tree edit distance score.

Similarity matrix $D$ given by $D_{ij} = \Delta(t_i, t_j)$.
\vspace{2em}
Given threshold $\tau \in [0,1]$ we say $t_i$ and $t_j$ are similar if $D_{ij}
\ge \tau$.

Group trees such that all elements in the group are within $\tau$.

# ANTLR similarity groups with $\tau = 0.01$

10 similarity groups from ANTLR source, when $\tau = 0.01$:

7 are patterns:

```
    $\metavar$;
```

```
    if( $\metavar$ ) $\metavar$;
```

```
    if( $\metavar$ ) { $\metavar$ } $\metavar$;
```

```
    return $\metavar$;
```

```
    for( $\metavar$ $\metavar$ : $\metavar$ ) $\metavar$;
```

```
    for( $\metavar$ = $\metavar$; $\metavar$ < $\metavar$; $\metavar$ ) $\metavar$;
```

```
    throw RuntimeException( $\metavar$ + $\metavar$ );
```

# ANTLR similarity groups with $\tau = 0.01$

3 are constants (no \metavar s):

```
try {
  walker.grammarSpec();
} catch (RecognitionException re){
  ErrorManager.internalError("bad grammar AST structure",re);
}
```

\hrule

```
while(sp !=
      StackLimitedNFAToDFAConverter.NFA_EMPTY_STACK_CONTEXT)
{
  n++;
  sp = sp.parent;
}
```

\hrule

```
switch(gtype) {
  case ANTLRParser.LEXER_GRAMMAR:
    return legalLexerOptions.contains(key);
  case ANTLRParser.PARSER_GRAMMAR:
    return legalParserOptions.contains(key);
  case ANTLRParser.TREE_GRAMMAR:
    return legalTreeParserOptions.contains(key);
  default:
    return legalParserOptions.contains(key);
}
```

# Workflow

\begin{center}

  \includegraphics[height=0.8\textheight]{figures/workflow.pdf}

\end{center}


# Antiunification

$$
au\left(\;\raisebox{1.5em}{\Tree[.A [.B ] [.C ]]}\quad,\quad\raisebox{1.5em}{\Tree[.A [.B [.D ]] [.F ]]}\;\right)
  = \left(\raisebox{1em}{\Tree[.A [.\metavar_1 ] [.\metavar_2 ]]},subst_l ,subst_r \right)
$$

where,
\begin{align*}
subst_l &= \{\square_1 \mapsto B\;,\; \square_2 \mapsto C\} \\\\
subst_r &= \{\square_1 \mapsto \raisebox{0.5em}{\Tree[.B [.D ]]}\;,\; \square_2 \mapsto F\}
\end{align*}

# Similarity groups versus threshold

What happens to similarity groups when we vary the threshold?

\begin{center}
\includegraphics[width=0.9\textwidth]{figures/clojure-number-of-modifications.pdf}


Number of additions, deletions, and modifications by threshold for the Clojure source.
\end{center}

# Patterns as a function of threshold

Generic Loop pattern, $\tau = 0.15$:

```
for ($\metavar$ = $\metavar$; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

Loop counter is initialized to zero, $\tau = 0.25$:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

Loop termination criteria becomes more specific, $\tau = 0.35$:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$.$\metavar$; $\metavar$) {
    $\metavar$
}
```

# Future work

  * We only consider structural patterns

      * Example: We don't detect design patterns

  * Not semantically aware

      * Example: changing the name of a loop variable leads to \metavar

  * Generate rewrite rules based on before and after patterns
  * Use patterns for searching as a structural `grep`-like mechanism
  * Correlate patterns with bug fixes

# Thank you!

\begin{center}
\Large Questions?
\end{center}
\vspace{2em}
\tiny\emph{
This work was supported in part by the US Department
of Energy Office of Science, Advanced Scientic Computing
Research contract no. DE-SC0004968. Additional support
was provided by Galois, Inc.}
