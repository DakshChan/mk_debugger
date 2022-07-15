const express = require('express');
const path = require('path');
const cors = require('cors');
const tmp = require('tmp');
const formData = require('express-form-data');

const { spawn, spawnSync} = require('child_process');
const fs = require('fs');
const {parser: jsonlParser} = require('stream-json/jsonl/Parser');
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

app.get('/die', (req, res) => {
  if (racket !== null) {
    racket.kill();
  }
  racket = null;
  racketbuf = null;
  racket_stdout_num = 0;
  res.send("killed");
});

app.post('/code', (req, res) => {
  //TODO: reformat before sending back to client
  userCode = req.files.file.path;
  if (racket !== null) {
    racket.kill();
    racketbuf = null;
  }
  // let tmpRunnerFile = tmp.fileSync({postfix: '.rkt'});
  // console.log(tmpRunnerFile.name);
  // const header = `#lang racket\n\n(provide (all-defined-out))\n(require (file "${debugger_path}"))\n\n`.replaceAll("\\", "\\\\");
  // fs.writeFileSync(tmpRunnerFile.name, header);
  // fs.appendFileSync(tmpRunnerFile.name, fs.readFileSync(userCode).toString());
  //
  // userCode = tmpRunnerFile.name;

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

  // let solutions = parseInt(req.body.solutions);
  // solutions = solutions > -1 ? solutions : "#f";
  // let samples = parseInt(req.body.samples);
  // samples = samples > -1 ? samples : "#f";
  // let steps = parseInt(req.body.steps);
  // steps = steps > -1 ? steps : "#f";
  // let query = req.body.query;

  // let runStatement = `(run ${solutions} ${samples} ${steps} #t ${query})`;
  // let resumeStatement = `(resume ${solutions} ${samples} ${steps} #t)`;
  // console.log(runStatement);
  // console.log(resumeStatement);

  if (req.body.command === "run") { // run
    if (racket !== null) {
      racket.kill();
    }
    racket = spawn("racket.exe", ["repl.rkt"]);
    racket_stdout_num = 0;
    // racket.stdin.write(`(require (file "${debugger_path}"))\n`.replaceAll("\\", "\\\\"));
    // console.log(`(require (file "${debugger_path}"))`.replaceAll("\\", "\\\\"));
    // racket.stdin.write(`(require (file "${userCode}"))\n`.replaceAll("\\", "\\\\"));
    // console.log(`(require (file "${userCode}"))`.replaceAll("\\", "\\\\"));
    // racket.stdin.write(`${runStatement}\n`);

  } else { // resume
    if (racket === null) {
      res.status(400).send("No racket process running");
      return;
    }
    // racket.stdin.write(`${resumeStatement}\n`);
  }

  racketbuf = "";
  racket.stdout.removeAllListeners();
  racket.stderr.removeAllListeners();

  racket.stdin.write(JSON.stringify(req.body) + "\n");

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

  // racket.stdout.on('data', (data) => {
  //   console.log(`$stdout: ${data}`);
  //   racketbuf += data;
  //   let a;
  //   while ((a = racketbuf.split("> ")).length > 1) {
  //     const [f, ...rest] = a;
  //     console.log(`${racket_stdout_num} stdout: ${f} rest: ${rest.toString()}`);
  //     console.log(rest);
  //     if (racket_stdout_num > 2){
  //       let resultJSON = JSON.parse(f);
  //       resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== "stdin")
  //       resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.path = x.path.filter(y => y.source !== "stdin"); return x;});
  //       resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.stack = x.stack.filter(y => y.source !== "stdin"); return x;});
  //       resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.path = x.path.filter(y => y.source !== "stdin"); return x;});
  //       resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.stack = x.stack.filter(y => y.source !== "stdin"); return x;});
  //       res.json(resultJSON);
  //       res.end();
  //     }
  //     racketbuf = rest.join("> ")
  //     console.log(racketbuf);
  //     console.log(`${racket_stdout_num} racketbuf: ${racketbuf}`);
  //     racket_stdout_num++;
  //   }
  // });
  //
  // racket.stdout
  //   .pipe(split(JSON.parse))
  //   .on('data', (data) => {
  //     console.log(`$stdout: ${data}`);
  //   })
  //   .on('error', (err) => {
  //     console.log(`$stdout: ${err}`);
  //   });

  racket.stdout
    .on('data', (data) => {
      console.log(`$stdout: ${data}`);
      racketbuf += data;
      try {
        const resultJSON = JSON.parse(racketbuf);
        console.log(JSON.stringify(resultJSON));
        resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
        resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
        resultJSON["rejected-states"] = resultJSON["rejected-states"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
        resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.path = x.path.filter(y => y.source !== false); return x;});
        resultJSON["solutions"] = resultJSON["solutions"].map(x => {x.stack = x.stack.filter(y => y.source !== false); return x;});
        console.log(JSON.stringify(resultJSON));
        res.json(resultJSON);
        res.end();
      } catch (e) {
        // ignore cause incomplete json, keep buffering
      }
    });

  racket.stderr.on('data', (data) => {
    console.error(`stderr: ${data.toString()}`);
    res.status(400).send(data.toString());
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
