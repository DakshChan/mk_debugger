import {useEffect, useLayoutEffect, useState} from "react";
import Prism from "./prism.js";
import "./prism.css"
import ProgramPoint from "./ProgramPoint";
import md5 from "md5";
import "./CodeContainer.css";
import Temp from "./Temp";

export default function CodeContainer ({code, debug, codeHighlight, setPointDebug}) {
  const [parsedLines, setParsedLines] = useState([]);
  const [range, setRange] = useState({max: 0, min: 0});

  useEffect(() => {
    if (debug !== undefined) {
      setParsedLines(programPointParser());
    }
  }, [debug]);

  useLayoutEffect(() => {
    Prism.highlightAll();
  }, [code, parsedLines]);

  useEffect(() => {
    if (codeHighlight.info === "encounters") {
      let max = Number.MIN_SAFE_INTEGER;
      let min = Number.MAX_SAFE_INTEGER;

      if (debug !== undefined){
        for (let i = 0; i < debug["program-points"].length; i++) {
          max = debug["program-points"][i]["count"] > max ? debug["program-points"][i]["count"] : max;
          min = debug["program-points"][i]["count"] < min ? debug["program-points"][i]["count"] : min;
        }
      }
      setRange({max: max, min: min});
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
      <pre className="line-numbers"><code className="language-racket match-braces">
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
        <pre className="line-numbers"><code className="language-racket match-braces">
          {code.split("\n").map((line, index) => {
            return <span key={index}>{line + "\n"}</span>
          })
          }
        </code></pre>
      </div>
    )
  }
}
