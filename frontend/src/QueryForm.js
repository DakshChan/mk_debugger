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
    <div style={{display: "flex"}}>
      <p style={{margin: "0 0.2em", fontSize:"1.3em"}}>( run </p>
      <div>
        <div>
          <input type={"number"} placeholder={"solutions"} value={solutions}
                 onChange={event => setSolutions(event.target.value)}/>
        </div>
        <div>
          <input type={"number"} placeholder={"samples"} value={samples}
                 onChange={event => setSamples(event.target.value)}/>
        </div>
        <div>
          <input type={"number"} placeholder={"steps"} value={steps}
                 onChange={event => setSteps(event.target.value)}/>
        </div>
        <div>
          <input type={"text"} placeholder={"query variables"} value={queryVars}
                 onChange={event => setQueryVars(event.target.value)}/>
        </div>
      </div>
      <textarea type={"text"} placeholder={"query"} value={query}
                onChange={event => setQuery(event.target.value)}/>
      <p style={{margin: "0 0.2em", fontSize:"1.3em"}}>)</p>
    </div>
  );
}