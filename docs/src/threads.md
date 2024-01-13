# Threads

## Regular thread

A thread block creates a temporary script that runs alongside the current script, copying the current script variables to the new script

```
thread {
    // do stuff
}
```

## Child thread

Similar to a regular thread except the thread is killed when the current script is finished executing

```
cthread {
    // do stuff
}
```

## Exec/Exec Wait

The family of exec functions allow for launching a new script from inside another

The `exec` function launches a script without blocking the execution of the current script

```
exec(cool_script)
```

The id of the launched script can be stored in a variable

```
Var[0] = exec(cool_script)
```

The `exec_wait` function launches a script and blocks the execution of the current script until the child returns

```
exec_wait(cool_script)
```

## Misc functions

|Function|Arguments|Return value|Description|
|-|-|-|-|
|bind|script pointer, event type: int, collider id: int|trigger pointer|binds a script to run when an event is triggered|
|bind_lock|script pointer, event type: int, collider id: int, item list: int|trigger pointer|works similar to the `bind` function but displays a list showing selectable items|
|unbind|-|-|unbinds the current script from a trigger|
|kill|id: int|-|kills a script with the given id|
|set_priority|priority: int|-|sets the priority of a script|
|set_timescale|timescale: float|-|sets the timescale of a script|
|set_group|group: int|-|sets the group of a script|
|suspend_group|group: int|-|suspends all scripts in the given group|
|resume_group|group: int|-|resumes all scripts in the given group|
|suspend_others|group: int|-|suspends all scripts in the given group excluding the current script|
|resume_others|group: int|-|resumes all scripts in the given group excluding the current script|
|suspend_thread|id: int|-|suspends all scripts with the id as well as their children|
|resume thread|id: int|-|resumes all scripts with the id as well as their children|
|is_thread_running|id: int|bool|returns whether a script with the given id is in the current script list|
