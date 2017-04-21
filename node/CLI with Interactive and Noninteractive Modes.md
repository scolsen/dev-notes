A basic use of readline and process to run a CLI in either an interactive or noninteractive mode.

```js
const fs = require('fs');
const readline = require('readline');

const commands = {
  "exit" : exitFunction,
  "option" : myFunction
}

function parseArgs(arguments){
	let cmdarr;
	if(Array.isArray(arguments) !== true){
		cmdarr = arguments.split(' ');
	} else {
		cmdarr = arguments;
	}
	let c = cmdarr[0];
	let opts = cmdarr.slice(1, cmdarr.length);
	if(options.hasOwnProperty(c)){
		commands[c](opts);
	} else if (c === 'exit' || c === 'Exit' || c === 'quit' || c === 'Quit'){
		console.log("Exiting");
		process.exit(0);
	} else {
		console.log("Unrecognized command: " + c);
	}	
}

function interactive(){
	const mrl = readline.createInterface({
		input: process.stdin,
		output: process.stdout,
		prompt: '[myprompt] '
	  });
	  mrl.prompt();
	  mrl.on('line', (line)=>{
	    let clean = line.replace(/\s{2,}/g, ' ');
	    parseArgs(clean.trim());
	    mrl.prompt();
	  });
}

function main(){
	if(process.argv.length <= 2){
	  interactive();  
	} else {
		let args = process.argv.slice(2, args.length);
      parseArgs(args); 
	}	
}

main();
```

This CLI provides interactive and noninteractive modes. When the interactive mode is invoked, arguments are read on line input and must be split into an array. In interactive mode, we utilize node’s process module.

A commands object enables the CLI to run a function, passing the remainder arguments as the function arguments, based on the first input command by the user.

You can also export the main function as an anonymous function for invocation in some other module:

```js
module.exports = function (){
	if(process.argv.length <= 2){
	  interactive();  
	} else {
		let args = process.argv.slice(2, args.length);
      parseArgs(args); 
	}	
}
```