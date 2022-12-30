import QueryForm from "./QueryForm";
import {useEffect, useState} from "react";
import { v4 as uuidv4 } from 'uuid';
import {Button} from "@chakra-ui/react";
import {socket} from "./socket";

export default function QueryPanel({queries, setQueries}) {
  // const [results, setResults] = useState([]);
  const [forms, setForms] = useState([uuidv4()]);

  // useEffect(() => {
  //   console.log(queryData);
  //   console.log(queryData[0]);
  //   if (queryData['0'] !== undefined) {
  //     setQueries({...queryData['0']});
  //   }
  // }, [queryData]);

  const removeForm = (id, index) => {
    setForms(forms.filter((e) => e !== id));
    let {[index]: _, ...q} = queries;
    setQueries(q);
    socket.emit('kill', id, (data) => {
      console.log(data);
    });
  }

  return (
    <div className={"query-panel"} style={{display: "flex", flexDirection: "column", gap: "0.2em"}}>
      {
        forms.map((id, index) => {
          return <QueryForm key={id} id={id} index={index} removeForm={removeForm} setQueries={setQueries} queries={queries}/>;
        })
      }
      <div>
        <Button size={'sm'} onClick={() => {setForms([...forms, uuidv4()])}}>Add Query</Button>
      </div>
    </div>
  );
}