Synthetic Script
====

* Skein
* Thread
* Bind/Knot/Word/Joint/Measure/Bar
* Twist/Coil (撚糸)
* Tuft/Tassel

## What's the problem when running collidable scripts?

When running these scripts independently, we need to orchestrate those script interpreters which shares single state DB.
This is too hard to implement, and not so efficient.

## Then, what should I do to run efficiently?

Here is four collidable sample script A, B, C and D:

````script
A_1: Read A_1
A_2: Oper A_1 V_
A_3: Oper A_1 V_
A_4: Read G_1
A_5: Oper A_1 G_1
A_6:
````

````script
B_1: Read B_1
B_2:
B_3:
B_4:
B_5: Write G_1

````

````script
C_1:
C_2:
C_3:
C_4:
C_5:
C_6:
C_7: Read G_1
````

````script
D_1:
D_2:
...
````

### Method #1 - Synthesize every collidable scripts as a single script

Synthesize every collidable scripts makes the problem simple.

````script
A_1:
B_1:
C_1:
A_2:
B_2:
C_2:
A_3:
B_3:
C_3:
A_4: Read G_1
B_4:
C_4:
A_5:
B_5: Write G_1

````

### Method #2 - Make a knot scripts (tassel) at executing variable colliding instruction

This method also synthesize script, but not as a single script.
Every script also exist as an independent script.
However, This scripts are knotted at variable colliding instruction, and I handles this as an twisted script.

#### Is there any difference between orchestrating multi-script interpreter?

Yes, and No.

The scripts are orchestrated by VC instructions.
However, this process is pre-analyzed and do not require inter-communications between interpreters.
Just executes given parted scripts freely.

## Implementation

* Transcript CEREScript with Event ID
* Shuffles/Reorders VC instructions in every scripts
* Knot scripts by shuffled VC instructions
* Shuffles/Reorders non-VC instructions (May not needed for Method #1 unless get jitter-time)
* Run the twisted script
  * Loop
    * Run each script-in-a-coil parallelly until meet a knot
    * Run a knot(VC) instruction

## Issues

### Why shuffles VC instruction?

I assume that the scripts runs concurrently means that is placed in same time-slot.
In other words, the scripts which are placed in different time-slot have no collide issues when running them time-slot by time-slot. (Inter-time-slot is another problem which have been discussed in prior-CERE)

The script only specifies that the (VC) instructions should be executed in the time-slot, but not specify the order among the instructions.
Therefore, we can reorder the execution order of the instructions.

Of course, there are some dependencies and priority among the scripts.
In this cases, we should separate the same type(dependency/priority level) scripts as a group.
Furthermore, 

### Do I need to knot *every* scripts when a script have no more VC instruction affected part?

This is an advanced issue.
We may explore that this approach is more effective. For example, whether shuffling separately lead slower result or not.
