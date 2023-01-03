# Frontend notes
The frontend is written in React using `create-react-app`.

We use socket.io to communicate with the backend.

`App.js` stores most of the state variables alongside the aggregation functions.

The `QueryPanel.js` holds mulitple `QueryForm.js` components,
which each store the result of the query.

`App.js` will then take each query result and aggregate it into a single result.

Prism.js is used for syntax highlighting. It has issues with performance since it messes with the dom.
The idea is to switch to [prism-react-renderer](https://www.npmjs.com/package/prism-react-renderer)
in the future.

We use chakra-ui for styling, it provides some sensible defaults, however it does mess with the styling of
the regular html elements.

Composition tree:
```
index.js
└── App.js
    ├── CodeContainer.js
    │   └── ProgramPoint.js
    ├── UploadCode.js
    ├── PointInfoPanel.js
    ├── StateInfoPanel.js
    └── QueryPanel.js
        └── QueryForm.js
            └── RunResume.js
            
```

## Uncompleted idea recap
- Run / Resume / Pause / Kill
  - Instead of running a query to completion, we can run multiple 10k (or more) step queries in succession, and stream the results to the frontend
  - This would allow the results to stream in, so the user can decide to pause it when it looks “good enough”
- Hovering on a line from the stack or path should filter / highlight the program points that take part of the stack:
  - When path/stack panel is visible, highlight program points that take part in the path/stack, dim the rest
  - Mousing over a line in the path / stack should only highlight the respective program point, and dim the rest
  - Conversely, clicking a program point could filter the Answers / Rejected states to show only the states that used that goal in the path
- Adjustable divider in between code and panels on the right
- Aggregation: Disjunction
  - Disjunction, take all program points of a, remove any program points that are contained in the other queries
- Footer for path/stack drawer explaining what the path and stack show
“The path shows the disjuncts taken to reach the solution”
- Show the encounters failures and successes per query, and have the total displayed up top
- Multiple file support
  - Users would be able to upload multiple files, and the code panel would allow them to switch the file they are viewing
  - The backend would need to be able to handle multiple files
