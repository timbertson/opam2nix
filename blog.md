This keeps confusing me, so I'll write it down for next time.

When connecting two processes with a pipe, I sometimes end up in the state where the first process has finished writing (and terminated), but my own process which is collecting the output of the _second_ process is still waiting for that process to end.

To demonstrate, consider this bash equivalent that acts a bit like [the count](TODO):

```
output="$(echo -e '1\n2\n3' | sed -e 's/$/! ah hah ha!/')"
echo -e "The result was:\n$output"
```

Which prints:

```
The result was:
1! ah hah ha!
2! ah hah ha!
```

Let's imagine we're doing this ourselves in something low-level, without the featureful [subprocess](https://docs.python.org/3/library/subprocess.html) library. That is, we need to manage pipes explicitly:

```

