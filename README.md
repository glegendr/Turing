# Turing
## Dependencies
The only dependencies you need are haskell and stack.   
To get them, check https://www.haskell.org/platform/
## Project
To start the project use:
`git clone https://github.com/glegendr/Turing.git; cd Turing; stack build`.   
Then you can use `stack exec ft_turing`
## Launch your machine
`stack exec ft_turing -- jsonFile input [-h.a]`    
To launch your machine, you need a [json file](#create-your-machine) and an [input](#input).   
You can also use extra flags.   
|short|long|description|
|:-:|:-:|:-:|
|-h|--help|display help message|
|-a|--animated|animate the machine's computing. You can give an extra speed like \"-a=50\" to speed up the program|    

![](unary_add.gif)
## Create your machine
To create your machine, you have to create a Json file with the following tags.    
|name|type|value|description|mandatory|
|:-:|:-:|:-:|:-:|:-:|
|name|`String`|`"name" : "my_machine_name"`|Machine's name|✔️|
|alphabet|`Array<String>`|`"alphabet" : [ "1", ".", "+"]`|The list of character you are free to use in your machine|✔️|
|blank|`String`|`"blank" : "."`|The blank character. Must be in the alphabet and not in the input|✔️|
|states|`Array<String>`|`"states" : [ "READ", "goto", "HALT"]`|The list of states you can use in `transitions`|✔️|
|initial|`String`|`"initial" : "READ"`|The initial state. Must be listed in `states`|✔️|
|final|`Array<String>`|`"finals" : [ "HALT" ]`|The list of all finishing states. Must be listed in `states`|✔️|
|transitions|`(String[(String:[String:String])])`|`"transitions" : { "READ" : [{..}]}`|A dictionnary of the machine’s transitions indexed by state name|✔️|
|transitions' state|`[(String:[String:String])]`|`"READ":[{..},{..}]`|An array of dictionnary of each transition of a state. Must be in `transitions`|✔️|
|generic_variable|`String`|`"generic_variable" : "TRUE"`|Allow generic variables. Defalut value:False||
|generic_transitions|`String`|`"generic_transitions" : "TRUE"`|Allow generic transitions. Defalut value:False||
### Transitions
The dictionaries inside `transitions` dictionnary are composed with the following tags.
|name|value|description|
|:-:|:-:|:-:|
|read|`"read" : "1"`|The character of the machine’s alphabet on the tape under themachine’s head|
|to_state|`"to_state": "goto`|The new state of the machine after the transition is done|
|write|`"write": "+"`|The character of the machine’s alphabet to write on the tape before moving the head|
|action|` action": "RIGHT"`|Movement of the head for this transition, either `LEFT`, or `RIGHT`|

Not all the states have to be in `transitions` dictionnary.
``` json
"transitions" : {
        "scanright": [
            { "read" : "1", "to_state": "scanright", "write": "1", "action": "RIGHT"},
            { "read" : "+", "to_state": "eraseone", "write": "+", "action": "RIGHT"},
            { "read" : "=", "to_state": "HALT", "write": ".", "action": "RIGHT"},
            { "read" : ".", "to_state": "HALT", "write": ".", "action": "RIGHT"}
        ],
        "eraseone": [
            { "read" : "+", "to_state": "HALT", "write": ".", "action": "RIGHT"},
            { "read" : "1", "to_state": "addone", "write": "+", "action": "LEFT"},
            { "read" : ".", "to_state": "eraseone", "write": ".", "action": "LEFT"},
            { "read" : "=", "to_state": "eraseone", "write": "=", "action": "LEFT"}
        ]
}
```
### Generic variable
The Generic variable is a way to avoid repetition in transitions.   
This variable is symbolized with `_` character and is setted up with the `generic_variable` tag.   
If the variable is set in `read` tag, that mean that every character that hasn't been setted up previously will go throw this transition.    
In an other hand, if the variable is set in `write` tag, that mean that the character that have been readed will be printed on the tag.   
The example above can be shortened by:"
``` json
"generic_variable" : "TRUE",
"transitions" : {
        "scanright": [
            { "read" : "1", "to_state": "scanright", "write": "1", "action": "RIGHT"},
            { "read" : "+", "to_state": "eraseone", "write": "+", "action": "RIGHT"},
            { "read" : "_", "to_state": "HALT", "write": ".", "action": "RIGHT"}
        ],
        "eraseone": [
            { "read" : "+", "to_state": "HALT", "write": ".", "action": "RIGHT"},
            { "read" : "1", "to_state": "addone", "write": "+", "action": "LEFT"},
            { "read" : "_", "to_state": "eraseone", "write": "_", "action": "LEFT"}
        ]
}
```
### Generic transitions
The Generic transisions is a way to avoid repetition in states.   
Theses transitions are generated automaticaly by the machine when called.   
Here are the generic transtions's variables:
|name|Usage|description|
|:-:|:-:|:-:|
|X|transition call|The character that has been inserted before the transition name during the build. Can be empty (Default:`blank`)|
|Y|transition call|The character that has been inserted after the transition name during the build. Can be empty (Default:`blank`)|
|F|transition call|The rest of the transitions if you build a list of transitions|
|D|transition call|Movement of the head for this transition. Can be empty (Default:`RIGHT`)|
|CURR|transition build|The current character|

`{ "read" : "_", "to_state": "CURR_goToX_RIGHT_CURR_replace_._toEnd", "write": "_", "action": "RIGHT"}`    
This line above will call transition `goToX` with
- `X` = current character   
- `D` = Right   
- `Y` = current character    
- `F` = `_replace_._toEnd`    
The [example here](jsonFiles/generic_transitions_ex.json) will replace the second occurence of the first character with `.` and go to the end of the tape.     
To create a generic transitions list, you __MUST__ call your first building transition `READ`.   
## Input
The input is the initial tape of your machine.    
She __must__ not countain the blank character in.   
## Universal Turing Machine
This is a list of input solving precedent problems with the Universal Turing Machine:    
- Unary_add : `"/+iL>gR+sR=rL+rL1d+|>1+1="`
- Unary_sub : `"/-iR>gR=oLBrLBmL-rL=gL-rR-gR=oL=oL1mL-gL-oR-rR1rR=rLBrL=gL-oR-dL-|>11-1="`
- 02n : `"/0iR>gR0GBMngRBoL0oL0rLBrLBG>Myd0|>000000"`
- on1n : `"/0iR>gR0oLBMyrRBgRBoLBoLBMnG0MnrLBG0MnCLBD|/1iR>SR1gRBoLBoL0MnrLBMngLBoRBoR1MnrRBMyoRBMngR1oLBCLBD|>00111"`
