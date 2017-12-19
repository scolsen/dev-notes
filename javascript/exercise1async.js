let gotResponses = {};

function fakeAjax(url,cb) {
  var fake_responses = {
    "file1": "The first text",
    "file2": "The middle text",
    "file3": "The last text"
  };
  var randomDelay = (Math.round(Math.random() * 1E4) % 8000) + 1000;

  console.log("Requesting: " + url);

  setTimeout(function(){
     cb(fake_responses[url], url);
  },randomDelay);
}

function output(text) {
    console.log(text);
}

function addRes(key, text) {
  gotResponses[key] = text;    
}

// **************************************
// The old-n-busted callback way

function getFile(file) {
    fakeAjax(file,function(text, url){
      gotResponses[url] = text;
      if(Object.keys(gotResponses).length == 3) {
          output(gotResponses["file1"]);
          output(gotResponses["file2"]);
          output(gotResponses["file3"]);
        }
    });
}

// request all files at once in "parallel"
getFile("file1");
getFile("file2");
getFile("file3");
