% Identifying change patterns in software history
% Jason Dagit ; Matthew Sottile
% Galois, Inc

# Motivation

**Dream Tool:** Finds meaningful patterns in source code and edit history to
support people with the following roles:

* Language Designers
* Programmers
* Managers

# Motivation

## Traditional line-based diff

Pro: diff is very general and programming language agnostic

Con: diff is not structurally aware:

```
if( foo ){           if( foo )
  bar;               {
}                      bar;
                     }
```

# Motivation

\includegraphics[height=0.8\textheight]{slide-figures/opendiff.png}

# Motivation

What can language designers learn from patterns?

Common looping pattern with loop counter initialized to zero:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

  * Highlights language features that get used together
  * We also want to see how source code *changes*

# Motivation

How can programmers benefit from patterns?

Before:

```
if( lock.acquire($\metavar$) ){
  $\metavar$
}
```

After:

```
while( lock.acquire($\metavar$) ){
  $\metavar$
}
```

  * Similar edits to two or more files $\Rightarrow$ might be a related
change

  * Directs our attention to take a closer look

  * *Only* a heuristic arugment, but humans are good at working with
such information

# Approach

  **Key Idea:** We can find *structural patterns* by generalizing *sufficiently
similar* difference trees.

  * Difference trees computed using structural diff of AST

  * Similarity is measured using a tree edit distance score

  * Generalization is accomplished through antiunification

# Workflow

\begin{center}

  \includegraphics[height=0.8\textheight]{figures/workflow.pdf}

\end{center}

# ATerms

\setlength{\grammarindent}{8em}
\begin{grammar}
<aterm> ::= `AAppl' $\langle$string$\rangle$ $\langle$aterm-list$\rangle$
\alt `AList' $\langle$aterm-list$\rangle$
\alt `AInt' $\langle$int$\rangle$

<aterm-list> ::= $\langle$aterm$\rangle$ $\langle$aterm-list$\rangle$
\alt $\epsilon$
\end{grammar}

* Generic tree structure
* Easy to modify parsers to generate ATerms

# Structural Diff Example

$$
treediff\left(\raisebox{1.5em}{\Tree[.A [.B ] [.C ]]},\raisebox{1.5em}{\Tree[.A [.B [.D ] ] [.F ]]}\right) = \raisebox{1.5em}{\Tree[.A [.B [.lefthole(D) ] ] [.mismatch(C,F) ]]}
$$

# Similarity Grouping

We define the similarity score by:

$$\Delta(t_a, t_b) := \frac{min(treediff(t_a, t_b),treediff(t_b, t_a))}{max(|t_a|,|t_b|)}$$

Distance matrix $D$ given by $D_{ij} = \Delta(t_i, t_j)$.

# Similarity Grouping

\begin{center}
\includegraphics[width=0.9\textwidth]{figures/distmatrix-0-9.png}

Similarity groups from the ANTLR repository with similarity threshold greater than 0.9
\end{center}

# Clojure

\begin{center}
\includegraphics[width=\textwidth]{figures/clojure-number-of-modifications.pdf}


Number of additions, deletions, and modifications by threshold for the Clojure source.
\end{center}


# ANTLR

\begin{center}
\includegraphics[width=\textwidth]{figures/antlr-number-of-modifications.pdf}

Number of additions, deletions, and modifications by threshold for the ANTLR source.
\end{center}

# Antiunification Example

$$
au\left(\raisebox{1.5em}{\Tree[.A [.B ] [.C ]]},\raisebox{1.5em}{\Tree[.A [.B [.D ]] [.F ]]}\right)
  = \left(\raisebox{1em}{\Tree[.A [.?_1 ] [.?_2 ]]},subst_l ,subst_r \right)
$$

where,
\begin{align*}
subst_l &= \{?_1 := B, ?_2 := C\} \\
subst_r &= \{?_1 := \raisebox{0.5em}{\Tree[.B [.F ]]}, ?_2 := F\}
\end{align*}

# Patterns as a function of threshold

Generic Loop pattern:

```
for ($\metavar$ = $\metavar$; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

Increase similarity requirement and the loop counter is initialized to zero:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

Increase it again and the loop termination criteria becomes more specific:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$.$\metavar$; $\metavar$) {
    $\metavar$
}
```

# Example from Clojure: Related edits

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

# Shortcomings:

  * Bias in dynamic programming of Yang's algorithm
  * We only consider structural patterns
  * Not semantically aware

#

\Large\begin{center}
 Thank you!
\end{center}
