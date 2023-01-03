import { useEffect, useLayoutEffect, useState } from "react";
import Prism from "./prism.js";
import "./prism.css"
import ProgramPoint from "./ProgramPoint";
import md5 from "md5";
import "./CodeContainer.css";

export default function CodeContainer ({code, debug, codeHighlight, pointDebug, setPointDebug}) {
  const [parsedLines, setParsedLines] = useState([]);
  const [range, setRange] = useState({encounters: {max: 0, min: 0}, failures: {max: 0, min: 0},
    successes: {max: 0, min: 0}, failRatio: {max: 0, min: 0}, successRatio: {max: 0, min: 0}});
  const [scroll, setScroll] = useState(0);

  function handleScroll(e) {
    setScroll(e.target.scrollTop);
  }

  useLayoutEffect(() => {
    let a = document.getElementById("code-container-main");
      if (a !== null) {
        a.scroll({top: scroll});
      }
  }, [code, codeHighlight, scroll]);

  useEffect(() => {
    if (debug !== undefined) {
      let programPointList = debug["program-points"];
      setParsedLines(programPointParser(code, programPointList));
    } else {
      setParsedLines([]);
    }
  }, [code, debug]);

  useLayoutEffect(() => {
    Prism.highlightAll();
  }, [code, parsedLines, codeHighlight, range]);

  useEffect(() => {
    let maxEncounter = Number.MIN_SAFE_INTEGER;
    let minEncounter = Number.MAX_SAFE_INTEGER;
    let maxFailure = Number.MIN_SAFE_INTEGER;
    let minFailure = Number.MAX_SAFE_INTEGER;
    let maxSuccess = Number.MIN_SAFE_INTEGER;
    let minSuccess = Number.MAX_SAFE_INTEGER;
    let maxFailRatio = Number.MIN_SAFE_INTEGER;
    let minFailRatio = Number.MAX_SAFE_INTEGER;
    let maxSuccessRatio = Number.MIN_SAFE_INTEGER;
    let minSuccessRatio = Number.MAX_SAFE_INTEGER;

    if (debug !== undefined){
      for (let i = 0; i < debug["program-points"].length; i++) {
        let programPoint = debug["program-points"][i];
        maxEncounter = Math.max(programPoint.count, maxEncounter);
        minEncounter = Math.min(programPoint.count, minEncounter);
        maxFailure = Math.max(programPoint.fails, maxFailure);
        minFailure = Math.min(programPoint.fails, minFailure);
        maxSuccess = Math.max(programPoint.successes, maxSuccess);
        minSuccess = Math.min(programPoint.successes, minSuccess);
        let fs = (programPoint.fails + programPoint.successes)
        if (fs === 0) {
          fs = 1;
        }
        maxFailRatio = Math.max((programPoint.fails / fs), maxFailRatio);
        minFailRatio = Math.min((programPoint.fails / fs), minFailRatio);
        maxSuccessRatio = Math.max((programPoint.successes / fs), maxSuccessRatio);
        minSuccessRatio = Math.min((programPoint.successes / fs), minSuccessRatio);
      }
    }
    setRange({
      encounters: {max: maxEncounter, min: minEncounter},
      failures: {max: maxFailure, min: minFailure},
      successes: {max: maxSuccess, min: minSuccess},
      failRatio: {max: maxFailRatio, min: minFailRatio},
      successRatio: {max: maxSuccessRatio, min: minSuccessRatio}
    });
  }, [debug]);

  if (debug !== undefined) {
    return (
      <div className="code-container" key={md5(code + JSON.stringify(debug) + JSON.stringify(codeHighlight))}>
        <pre className="line-numbers"><code id="code-container-main" className="language-racket match-braces" onScroll={handleScroll}>
          {
            parsedLines.map((line, index) => {
              if (line.data !== undefined) {
                return <ProgramPoint key={md5(line.text) + index} data={line.data} range={range} codeHighlight={codeHighlight} pointDebug={pointDebug} setPointDebug={setPointDebug}>{line.text}</ProgramPoint>
              } else {
                return <span key={md5(line.text) + index}>{line.text}</span>
              }
            })
          }
        </code></pre>
      </div>
    )
  } else {
    return (
      <div className="code-container" key={md5(code)}>
        <pre className="line-numbers"><code id="code-container-main" className="language-racket match-braces" onScroll={handleScroll}>
          {code.split("\n").map((line, index) => {
            return <span key={md5(line) + index}>{line + "\n"}</span>
          })
          }
        </code></pre>
      </div>
    )
  }
}

function programPointParser(code, programPointList) {
  let resList = [];
  programPointList.sort((a, b) =>  a.syntax.position - b.syntax.position);
  let end = 1, start = 1;
  for (let programPoint of programPointList) {
    programPoint = structuredClone(programPoint);
    start = programPoint.syntax.position;
    if (start - end > 0) {
      resList.push({"key": end, "text": code.substring(start-1, end-1), "data": undefined});
    }
    end = start + programPoint.syntax.span;
    let fs = (programPoint.fails + programPoint.successes)
    if (fs === 0) {
      fs = 1;
    }
    programPoint.failRatio = programPoint.fails / fs;
    programPoint.successRatio = programPoint.successes / fs;
    resList.push({"key": start, "text": code.substring(start-1, end-1), "data": programPoint});
  }
  if (end !== code.length) {
    resList.push({"key": end, "text": code.substring(end, code.length), "data": undefined});
  }
  return resList
}
