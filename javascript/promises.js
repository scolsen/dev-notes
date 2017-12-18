//How to use promises.
/*
* Promises serve the same function as callbacks but are composable.
*/

//A basic promise stolen from Google's doc on the subject: https://developers.google.com/web/fundamentals/primers/promises

let promise = new Promise(function (resolve, reject) {
  //do stuff
  if(stuffWorked) {
    resolve("stuff worked");  
  }
  else {
    reject(Error("stuff didn't work"));   
  }
}
