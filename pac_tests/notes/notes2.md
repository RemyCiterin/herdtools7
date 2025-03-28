# Pointer Authentication and Weak Memory Models

This note describe the memory model of the PAC instructions, and the relation
between PAC instructions and the dependency in the memory model. Also this note
focus on `FEAT_Pauth2`.

Pointer Authentication or PAC add three basic instructions: `pac* x, y`, `aut*
x, y` and `xpac x`, and adding PAC in herd7 can also introduce some changes in
the memory dependency of memory operations like loads and stores.


First each instructions of PAC add data dependcies between the inputs and the
outputs of the instructions. As example the program

```asm
(1) ldr x0,[x2]
(2) ldr x1,[x3]
    autda x0, x1
(3) ldr x4,[x0]
```

contain a data dependency between the event (1), (2) and the event (3) because
of the `autda` instruction, and the dependency graph is (at least) the following:


```mermaid
graph TD;
    A("R[x]=pac(z, da, 42)")
    B("R[y]=42")
    C("R[z]=97")

    A -->|data| C;
    B -->|data| C;
```


But this data dependency is not the only one.
In particular `aut*` may fail if `FEAT_FPAC` is present, and each memory operation
may fail if the PAC field of the address is not canonical (all ones or all
zeros), this implies that each check may add new events and new relations, in
particular intrinsic control dependencies.

Here is the example of the instruction `autda x0, x1`, if I understand correctly
the events generated by this instruciton and their dependencies must be the
following:

```mermaid
graph TD;
    A("R:X0=pac(x, da, 42)");
    B("R:X1=42");
    C("Branching(pac)");
    D("W:X0=x");

    A -->|iico_data| C;
    B -->|iico_data| C;
    B -->|iico_data| D;
    A -->|iico_data| D;
    C -->|iico_ctrl| D;
```

And in particular the output depend of `x0` and `x1` even if `x1` is not
necessary to compute the output in case of success and that the instruction
fault in case of failure. Because according to the documentation the output PAC
field must be the exclusive OR of `x0` and the hash of `x0` with the key `da`
and the modifier `x1` so their is a syntactic dependency in `x1`.

Also the loads and stores from virtual address must have the same intrinsic
control dependency because they have to check that the virtual address is valid
and raise a fault if this is not the case, as example for `ldr x1,[x0]`:

```mermaid
graph TD;
    A("R:X0=x");
    B("Branching(mmu)");
    C("R[x]=0");
    D("W:X1=0");

    A -->|iico_data| B;
    A -->|iico_data| C;
    B -->|iico_ctrl| C;
    C -->|iico_data| D;
```

And for `str x1, [x0]`:

```mermaid
graph TD;
    A("R:X0=x");
    B("Branching(mmu)");
    C("W[x]=0");
    D("R:X1=0");

    A -->|iico_data| B;
    A -->|iico_data| C;
    B -->|iico_ctrl| C;
    D -->|iico_data| C;
```
