const express = require('express');
const path = require('path');
const cors = require('cors');
const tmp = require('tmp');
const formData = require('express-form-data');

const { spawn, spawnSync} = require('child_process');
const fs = require('fs');
const testdata = JSON.parse(fs.readFileSync("./sample_response.json"));

const app = express();
const port = process.env.PORT || 3000;


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
let racket = null;
let racketbuf = null;
let racket_stdout_num = 0;

const debugger_path = "C:\\Users\\daksh\\WebstormProjects\\mk_debugger\\debugger\\mk-fo.rkt";


app.post('/code', (req, res) => {
  //TODO: reformat before sending back to client
  userCode = req.files.file.path;
  if (racket !== null) {
    racket.kill();
    racketbuf = null;
  }
  let tmpRunnerFile = tmp.fileSync({postfix: '.rkt'});
  console.log(tmpRunnerFile.name);
  const header = `#lang racket\n\n(provide (all-defined-out))\n(require (file "${debugger_path}"))\n\n`.replaceAll("\\", "\\\\");
  fs.writeFileSync(tmpRunnerFile.name, header);
  fs.appendFileSync(tmpRunnerFile.name, fs.readFileSync(userCode).toString());

  userCode = tmpRunnerFile.name;

  res.sendFile(userCode);

});

//TODO: use sockets so computation can be interrupted

app.post('/debug', (req, res) => {
  console.log(JSON.stringify(req.body));

  if (userCode === null) {
    res.status(400).send("No code uploaded");
    return;
  }



  //TODO: this should be on the client side so that user can see full query being run

  let solNum = parseInt(req.body.solutionNum);
  solNum = solNum > -1 ? solNum : "#f";
  let sampleNum = parseInt(req.body.sampleNum);
  sampleNum = sampleNum > -1 ? sampleNum : "#f";
  let stepNum = parseInt(req.body.stepNum);
  stepNum = stepNum > -1 ? stepNum : "#f";
  let query = req.body.query;

  let runStatement = `(run ${solNum} ${sampleNum} ${stepNum} #t ${query})`;
  let resumeStatement = `(resume ${solNum} ${sampleNum} ${stepNum} #t)`;
  console.log(runStatement);
  console.log(resumeStatement);

  if (req.body.runResume) { // run
    if (racket !== null) {
      racket.kill();
    }
    racket = spawn("racket.exe", ["-i"]);
    racket_stdout_num = 0;
    racketbuf = "";
    racket.stdin.write(`(require (file "${debugger_path}"))\n`.replaceAll("\\", "\\\\"));
    console.log(`(require (file "${debugger_path}"))`.replaceAll("\\", "\\\\"));
    racket.stdin.write(`(require (file "${userCode}"))\n`.replaceAll("\\", "\\\\"));
    console.log(`(require (file "${userCode}"))`.replaceAll("\\", "\\\\"));
    racket.stdin.write(`${runStatement}\n`);

  } else { // resume
    if (racket === null) {
      res.status(400).send("No racket process running");
      return;
    }
    racket.stdin.write(`${resumeStatement}\n`);
  }

  // const debugger_path = "C:\\Users\\daksh\\WebstormProjects\\mk_debugger\\debugger\\mk-fo.rkt";
  //
  // const code_statement = `#lang racket\n\n(require (file "${debugger_path}"))\n(include (file "${userCode}"))\n${runStatement}`.replaceAll("\\", "\\\\");
  // console.log(code_statement);

  // const racket = spawnSync('racket.exe', [tmpRunnerFile.name], {maxBuffer: Number.MAX_SAFE_INTEGER});
  //
  // if (racket.stderr.toString() !== "") {
  //   res.status(400).send(racket.stderr.toString());
  // } else {
  //   console.log(racket.stdout.toString().length);
  //   let resultJSON = JSON.parse(racket.stdout.toString());
  //   // console.log(racket.stdout.toString());
  //   //filter out initial debugger query
  //
  //
  //   // console.log(JSON.stringify(resultJSON));
  //   res.send(JSON.stringify(resultJSON));
  // }

  racket.stdout.on('data', (data) => {
    console.log(`$stdout: ${data}`);
    racketbuf += data;
    let a;
    while ((a = racketbuf.split("> ")).length > 1) {
      const [f, ...rest] = a;
      console.log(`${racket_stdout_num} stdout: ${f} rest: ${rest.toString()}`);
      console.log(rest);
      if (racket_stdout_num > 2){
        let resultJSON = JSON.parse(f);
        resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== "stdin")
        resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.path = x.path.filter(y => y.source !== "stdin"); return x;});
        resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.stack = x.stack.filter(y => y.source !== "stdin"); return x;});
        resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.path = x.path.filter(y => y.source !== "stdin"); return x;});
        resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.stack = x.stack.filter(y => y.source !== "stdin"); return x;});
        res.json(resultJSON);
        res.end();
      }
      racketbuf = rest.join("> ")
      console.log(racketbuf);
      console.log(`${racket_stdout_num} racketbuf: ${racketbuf}`);
      racket_stdout_num++;
    }
  });

  racket.stderr.on('data', (data) => {
    console.error(`stderr: ${data.toString()}`);
    res.status(400).send(data.toString());
    data.
    racket.kill();
    racket = null;
  });
});

app.get('(/*)?', (req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`)
})
