import {useEffect, useLayoutEffect, useState} from "react";
import Prism from "./prism.js";
import "./prism.css"
import ProgramPoint from "./ProgramPoint";
import md5 from "md5";
import "./CodeContainer.css";
import Temp from "./Temp";

export default function CodeContainer ({code, debug, codeHighlight, setPointDebug}) {
  const [parsedLines, setParsedLines] = useState([]);
  const [range, setRange] = useState({encounters: {max: 0, min: 0}, failures: {max: 0, min: 0}, successes: {max: 0, min: 0}});
  const [scroll, setScroll] = useState(0);

  function handleScroll(e) {
    setScroll(e.target.scrollTop);
  }

  useLayoutEffect(() => {
    console.log("scroll");
    let a = document.getElementById("code-container-main");
    console.log(a);
      if (a !== null) {
        a.scroll({top: scroll});
        console.log(scroll);
      }
  }, [code, codeHighlight]);

  useEffect(() => {
    if (debug !== undefined) {
      setParsedLines(programPointParser());
    }
  }, [debug]);

  useEffect(() => {
    Prism.highlightAll();
  }, [code, parsedLines, codeHighlight, range]);

  useEffect(() => {
    console.log(code);
  }, [code]);

  useEffect(() => {
    if (codeHighlight.info === "encounters") {
      let maxEncounter = Number.MIN_SAFE_INTEGER;
      let minEncounter = Number.MAX_SAFE_INTEGER;
      let maxFailure = Number.MIN_SAFE_INTEGER;
      let minFailure = Number.MAX_SAFE_INTEGER;
      let maxSuccess = Number.MIN_SAFE_INTEGER;
      let minSuccess = Number.MAX_SAFE_INTEGER;

      if (debug !== undefined){
        for (let i = 0; i < debug["program-points"].length; i++) {
          maxEncounter = debug["program-points"][i]["count"] > maxEncounter ? debug["program-points"][i]["count"] : maxEncounter;
          minEncounter = debug["program-points"][i]["count"] < minEncounter ? debug["program-points"][i]["count"] : minEncounter;
          maxFailure = debug["program-points"][i]["fails"] > maxFailure ? debug["program-points"][i]["fails"] : maxFailure;
          minFailure = debug["program-points"][i]["fails"] < minFailure ? debug["program-points"][i]["fails"] : minFailure;
          maxSuccess = debug["program-points"][i]["successes"] > maxSuccess ? debug["program-points"][i]["successes"] : maxSuccess;
          minSuccess = debug["program-points"][i]["successes"] < minSuccess ? debug["program-points"][i]["successes"] : minSuccess;
        }
      }
      setRange({encounters: {max: maxEncounter, min: minEncounter},
        failures: {max: maxFailure, min: minFailure}, successes:
      {max: maxSuccess, min: minSuccess}});
    } else if (codeHighlight.info === "failures") {
      //tbd when failure data is available
    }
  }, [debug, codeHighlight]);

  const programPointParser = () => {
    let resList = [];
    if (debug !== undefined) {
      let programPointList = debug["program-points"];
      programPointList.sort((a, b) =>  a.syntax.position - b.syntax.position);
      let end = 1, start = 1;
      for (const programPoint of programPointList) {
        start = programPoint.syntax.position;
        if (start - end > 0) {
          resList.push({"key": end, "text": code.substring(start-1, end-1), "data": undefined});
        }
        end = start + programPoint.syntax.span;
        resList.push({"key": start, "text": code.substring(start-1, end-1), "data": programPoint});
      }
      if (end !== code.length) {
        resList.push({"key": end, "text": code.substring(end, code.length), "data": undefined});
      }

    }
    return resList
  }

  if (debug !== undefined) {
    return (
      <div className="code-container" key={md5(code + JSON.stringify(debug) + JSON.stringify(codeHighlight))}>
        <pre className="line-numbers"><code id="code-container-main" className="language-racket match-braces" onScroll={handleScroll}>
          {
            parsedLines.map((line, index) => {
              if (line.data !== undefined) {
                return <ProgramPoint key={md5(line.text + index.toString())} data={line.data} range={range} codeHighlight={codeHighlight} setPointDebug={setPointDebug}>{line.text}</ProgramPoint>
              } else {
                return <span key={md5(line.text + index.toString())}>{line.text}</span>
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
            return <span key={index}>{line + "\n"}</span>
          })
          }
        </code></pre>
      </div>
    )
  }
}
