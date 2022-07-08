const express = require('express');
const path = require('path');
const cors = require('cors');
const tmp = require('tmp');

const { spawn, spawnSync} = require('child_process');
const fs = require('fs');
const testdata = JSON.parse(fs.readFileSync("./sample_response.json"));

const app = express();
const port = process.env.PORT || 3000;

const formData = require('express-form-data');

app.use(formData.parse());

app.use(express.static(path.join(__dirname, 'build')))

// cors only being used when running react server, remove after react build
app.use(cors({
  origin: '*'
}));

app.get('/code', (req, res) => {
  res.send(testdata);
})

let userCode = null;
app.post('/code', (req, res) => {
  //TODO: reformat before sending back to client
  userCode = req.files.file.path;
  // res.send(userCode);
  res.sendFile(req.files.file.path)
});

//TODO: use sockets so computation can be interrupted

app.post('/debug', (req, res) => {
  if (userCode === null) {
    res.status(400).send("No code uploaded");
    return;
  }

  //TODO: this should be on the client side so that user can see full query being run

  let runStatement = "";
  if (parseInt(req.body.solutionNum) === -1) {
    if (parseInt(req.body.sampleNum) === -1) {
      runStatement = `(run*/debug*/json ${req.body.query})`;
    } else {
      runStatement = `(run*/debug/json ${req.body.sampleNum} ${req.body.query})`;
    }
  } else {
    if (parseInt(req.body.sampleNum) === -1) {
      runStatement = `(run/json ${req.body.solutionNum} ${req.body.solutionNum} ${req.body.query})`;
    } else {
      runStatement = `(run/json ${req.body.solutionNum} ${req.body.sampleNum} ${req.body.query})`;
    }
  }

  const debugger_path = "C:\\Users\\daksh\\WebstormProjects\\mk_debugger\\debugger\\mk-fo.rkt";
  let tmpRunnerFile = tmp.fileSync({postfix: '.rkt'});
  console.log(tmpRunnerFile.name);

  const code_statement = `#lang racket\n\n(require (file "${debugger_path}"))\n(include (file "${userCode}"))\n${runStatement}`.replaceAll("\\", "\\\\");
  console.log(code_statement);
  fs.writeFileSync(tmpRunnerFile.name, code_statement);

  const racket = spawnSync('racket.exe', [tmpRunnerFile.name], {maxBuffer: Number.MAX_SAFE_INTEGER});

  if (racket.stderr.toString() !== "") {
    res.status(400).send(racket.stderr.toString());
  } else {
    console.log(racket.stdout.toString().length);
    let resultJSON = JSON.parse(racket.stdout.toString());
    // console.log(racket.stdout.toString());
    //filter out initial debugger query
    resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== tmpRunnerFile.name)

    // console.log(JSON.stringify(resultJSON));
    res.send(JSON.stringify(resultJSON));
  }
});

app.get('(/*)?', (req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
