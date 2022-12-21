import QueryForm from "./QueryForm";
import {useEffect, useState} from "react";

export default function QueryPanel({setQueries}) {
  const [queryData, setQueryData] = useState({}); // '0': {numSolutions: 0, samples: 0, step: 0, queryVars: "(x y)", query: "(appendo x y '(1 2 3))"}

  useEffect(() => {
    console.log(queryData);
    console.log(queryData[0]);
    if (queryData['0'] !== undefined) {
      setQueries({...queryData['0']});
    }
  }, [queryData]);

  return (
    <div className={"query-panel"}>
      <QueryForm id={'0'} queryData={queryData} setQueryData={setQueryData}/>
    </div>
  );
}