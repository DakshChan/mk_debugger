import { useEffect, useState } from "react";

export default function ProgramPoint({children, data, range, codeHighlight, pointDebug, setPointDebug}) {
  const [color, setColor] = useState("argb(255, 0, 0, 100)");

  useEffect(() => {
    colorMap(setColor, codeHighlight, range, data);
  }, [setColor, codeHighlight, range, data]);

  return (
    <>
      <span className="program-point"
            onClick={() => {if (pointDebug === data) {setPointDebug(undefined)} else {setPointDebug(data)}}}
            style={{background: color, borderRadius: "0.3em", outlineStyle:"solid",
              outlineColor: "#000", outlineWidth:"0.1em"}}>
        {children}
      </span>
    </>
  );
}

function rangeMap(x, in_min, in_max, out_min, out_max) {
  if (in_min === in_max) {
    return out_max;
  } else {
    return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
  }
}

function colorMap(setColor, codeHighlight, range, data) {
  if (codeHighlight.info === "encounters") {
    let c = 255 - rangeMap(data.count, range.encounters.min, range.encounters.max, 0, 200);
    setColor(`rgba(${c}, ${c}, 255, 100%)`);
  } else if (codeHighlight.info === "failures") {
    let c = 255 - rangeMap(data.fails, range.failures.min, range.failures.max, 0, 200);
    setColor(`rgba(255, ${c}, ${c}, 100%)`);
  } else if (codeHighlight.info === "successes") {
    let c = 255 - rangeMap(data.successes, range.successes.min, range.successes.max, 0, 200);
    setColor(`rgba(${c}, 255, ${c}, 100%)`);
  } else if (codeHighlight.info === "successRatio") {
    let c = 255 - rangeMap(data.successRatio, range.successRatio.min, range.successRatio.max, 0, 200);
    setColor(`rgba(${c}, 255, ${c}, 100%)`);
  } else if (codeHighlight.info === "failRatio") {
    let c = 255 - rangeMap(data.failRatio, range.failRatio.min, range.failRatio.max, 0, 200);
    console.log(data.failRatio, range.failRatio.min, range.failRatio.max, c);
    setColor(`rgba(255, ${c}, ${c}, 100%)`);
  } else {
    setColor(`rgba(0, 0, 0, 0%)`);
  }
}
