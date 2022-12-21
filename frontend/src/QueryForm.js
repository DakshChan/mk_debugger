import {useEffect, useState} from "react";
export default function QueryForm({id, queryData, setQueryData}) {
  const [solutions, setSolutions] = useState("");
  const [samples, setSamples] = useState("");
  const [steps, setSteps] = useState("");
  const [queryVars, setQueryVars] = useState("");
  const [query, setQuery] = useState("");

  useEffect(() => {
    setQueryData({...queryData, [id]: {
      solutions: (solutions !== "") ? solutions : "#f",
      samples: (samples !== "") ? samples : "#f",
      steps: (steps !== "") ? steps : "#f",
        queryVars, query}
    });
  }, [setQueryData, solutions, samples, steps, queryVars, query, id]);

  return (
    <div style={{display: "flex", gap: "0.2em"}}>
      <p style={{margin: "0", fontSize:"1.3em"}}>( run </p>
      <div style={{display: "flex", flexDirection: "column", gap:"0.1em"}}>
          <input type={"number"} placeholder={"solutions"} value={solutions}
                 onChange={event => setSolutions(event.target.value)}/>
          <input type={"number"} placeholder={"samples"} value={samples}
                 onChange={event => setSamples(event.target.value)}/>
          <input type={"number"} placeholder={"steps"} value={steps}
                 onChange={event => setSteps(event.target.value)}/>
          <input type={"text"} placeholder={"query variables"} value={queryVars}
                 onChange={event => setQueryVars(event.target.value)}/>
      </div>
      <textarea type={"text"} placeholder={"query"} value={query}
                onChange={event => setQuery(event.target.value)}/>
      <p style={{margin: "0", fontSize:"1.3em"}}>)</p>
    </div>
  );
}