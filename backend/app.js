const express = require('express');
const path = require('path');
const cors = require('cors');
const tmp = require('tmp');
const formData = require('express-form-data');

const { spawn } = require('child_process');
const fs = require('fs');

const app = express();
const port = process.env.PORT || 3000;


app.use(formData.parse());

app.use(express.static(path.join(__dirname, 'build')))

// cors only being used when running react server, remove after react build
app.use(cors({
  origin: '*'
}));

let userCode = null;
let racket = null;
let racketBuf = null;

const debugger_path = "C:\\Users\\daksh\\WebstormProjects\\mk_debugger\\debugger\\mk-fo.rkt";

app.get('/die', (req, res) => {
  if (racket !== null) {
    racket.kill();
  }
  racket = null;
  racketBuf = null;
  res.send("killed");
});

app.post('/code', (req, res) => {
  //TODO: potentially reformat before sending back to client
  userCode = req.files.file.path;
  if (racket !== null) {
    racket.kill();
    racketBuf = null;
  }

  // let tmpRepl = tmp.fileSync({postfix: '.rkt'});
  // console.log(tmpRepl.name);
  // const header = `#lang racket\n\n(provide (all-defined-out))\n(require (file "${debugger_path}"))\n\n`.replaceAll("\\", "\\\\");
  // fs.writeFileSync(tmpRepl.name, header);
  // fs.appendFileSync(tmpRepl.name, fs.readFileSync(userCode).toString());

  res.sendFile(userCode);
});

app.post('/debug', (req, res) => {
  console.log(JSON.stringify(req.body));

  if (userCode === null) {
    res.status(400).send("No code uploaded");
    return;
  }

  racketBuf = "";
  if (racket !== null) {
    racket.stdout.removeAllListeners();
    racket.stderr.removeAllListeners();
  }

  if (req.body.command === "run") { // run
    if (racket !== null) {
      racket.kill();
    }
    racket = spawn("racket.exe", ["repl.rkt"]);

  } else { // resume
    if (racket === null) {
      res.status(400).send("No racket process running");
      return;
    }
  }

  racket.stdin.write(JSON.stringify(req.body) + "\n");

  racket.stdout.on('data', (data) => {
    // console.log(`$stdout: ${data}`);
    racketBuf += data;
    try {
      const resultJSON = JSON.parse(racketBuf);
      // console.log(JSON.stringify(resultJSON));
      resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
      resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
      resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
      resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
      resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
      // console.log(JSON.stringify(resultJSON));
      res.json(resultJSON);
      res.end();
    } catch (e) {
      // ignore because incomplete json, keep buffering
    }
  });

  racket.stderr.on('data', (data) => {
    console.error(`stderr: ${data.toString()}`);
    res.sendStatus(400);
    res.status(400).write(data.toString());
    res.end();

    racket.stdout.removeAllListeners();
    racket.stderr.removeAllListeners();
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
