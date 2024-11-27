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
of the `autda` instruction. But this data dependency is not the only one, in
particular `aut*` may fail if `FEAT_FPAC` is present, and each memory operation
may fail if the PAC field of the address is not canonical (all ones or all
zeros), this implies that each check may add new events and new relations, in
particular control dependencies.

Here is an example if I understand correctly with `FEAT_Pauth2` the requested
graph for the program `autda x0, x1` is (in case of success):

```mermaid
graph TD:
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

Because the output always depend of the two input registers as it is the XOR of
the value of `x0` and a value determined by `x0` and `x1`.

Also the loads and stores from virtual address must have the same dependencies
because they have to check that the virtual address is valid and raise a fault
if this is not the case, as example for `ldr x1,[x0]`:


```mermaid
graph TD:
    A("R:X0=x");
    B("Branching(mmu)");
    C("R[x]=0");
    D("W:X1=x");

    A -->|iico_data| B;
    A -->|iico_data| C;
    B -->|iico_ctrl| C;
    C -->|iico_data| D;
```


And this added branch may change the memory ordering because of
"dependency-observed-before" contain the relation "ctrl; [W]", so in the
following programs:

```asm
(1) ldr x1, [x0]
    str x2, [x1]
(2) str x3, [x4]
```

and

```asm
(1) ldr x1, [x0]
    autdza x1
(2) str x3, [x4]
```

the read event (1) must be globally ordered before the write event (2) because
of this control dependency.