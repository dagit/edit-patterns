% Identifying change patterns in software history
% Jason Dagit ; Matthew Sottile
% Galois, Inc

# Motivation

We would like to be able to take existing software projects and use the history
stored in the VCS to answer questions which may be important to software
developers, software project managers, and language designers.

# Motivation

**Idea:** Humans are good at processing and reasoning with patterns. How can we
distill patterns from source code to aid human understanding?

**Problem:** Finding meaningful patterns in large codebases is difficult. There
is often too much to look at.  Can we automate the process of finding
meaningful patterns in a large codebase?

# Example of something we can learn from a pattern

Can we tell how code evolves structurally?

* TODO: Text from the paper. Distill this into a slide:

Language designers may want to know whether specific syntactic constructs would
make the language more productive for users. Taking an example from Java, we
might consider the addition of the for-each loop construct. This feature could
be partially justified by doing an analysis of existing source code to
determine that most for-loops iterate over an entire collection. To strengthen
this argument, it would be insightful to know what is the impact of maintain-
ing the code without for-each. For example, if refactoring the code commonly
leads to editing the bounds to match the collection used, then the argument in
favor of adding for-each is strengthened, as now it helps to prevent a class of
bugs where programmers forget to update the bounds. We demonstrate the
detection of loop patterns within ANTLR in Section 3.2.

# Example of loop pattern

Common looping pattern where loop counter is initialized to zero:

```
for ($\metavar$ = 0; $\metavar$ < $\metavar$; $\metavar$) {
    $\metavar$
}
```

# Example of something we can reason about using patterns

Can we tell when changes are related?

* TODO: Distill this into a slide:

Software developers joining a new project or team are expected to learn the
source code that they will be working with. We would like to provide these
programmers with tools that aid them in this task by allowing them to see what
types of changes other team members have made in the past. Software developers
may also want to compare the changes that happen in response to related bugs,
hoping to find opportunities to improve software quality, either by searching
for buggy patterns in the source code or making a tool to detect the pattern in
new code. We demonstrate the detection of generic patterns within the Clojure
compiler in Section 3.3.

# Example of reasoning

  * Similar edits to two or more files $\Rightarrow$ might be a related
change

  * Directs our attention and tells us to take a closer look

  * *Only* a heuristic arugment, but we claim humans are good at working with
such information

# Example of reasoning

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
>-                if(RT.isReduced(init))
>-                        return ((IDeref)init).deref();
-               }
-           }
+        }
     return init;
 }
```

# Secret Sauce

  **Key Idea:** We can find *structural patterns* by generalizing *sufficiently
similar* difference trees.

  * Difference trees computed using structural diff of AST

  * Similarity is measured using a tree edit distance score (from Yang's algorithm)

  * Generalization is accomplished through antiunification

  Workflow: Structural Diff $\rightarrow$ Group by Similarity $\rightarrow$ Antiunify

# Structural Diff Example

  * Small trees diffed

# Similarity Grouping Example

  * Give the intuition with an example

  We define the similarity score by $\Delta(t_a,t_b)$:

$$\Delta(t_a, t_b) := \frac{min(d(t_a, t_b),d(t_b, t_a))}{max(|t_a|,|t_b|)}$$
where $d(t_a, t_b)$ is the tree edit distance score. We use $min$ and $max$ to
obtain symmetry.

# Antiunification Example

  * Two small trees, step through the algorithm
  * Show the substitutions

# Putting it all together

  * review example of whole pipeline
    * Structural diff gave us:
    * Similarity grouping resulted in:
    * Antiunification generated:

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

# Shortcomings:

  * Bias in dynamic programming of Yang's algorithm
  * We only consider structural patterns
  * Not semantically aware

#

  Thank you!

  Questions?

# Backup Slides

# Unification vs. Antiunification

  * Example of how they differ
  * This slide is here incase someone asks. I should probably not bring it up
    otherwise.

# Relationship to Clone Digger

  * How are we similar, how are we different?
  * Comparison to "Clone Detection Using Abstract Syntax Trees" by Ira Baxter, Andrew Yahin et al
