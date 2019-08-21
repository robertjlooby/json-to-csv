# json-to-csv

A tool for converting JSON values to comma-separated values (CSVs).

JSON is read from standard input and written to standard output. All valid JSON values will produce valid CSV output. There are some cases that won't work well (such as having multiple arrays at the same "level" in the JSON or having many differently shaped objects in an array), but the goal is that any JSON that "seems like it would convert nicely to a CSV" does.

As an example from [this football dataset](https://raw.githubusercontent.com/openfootball/football.json/master/2016-17/en.1.json):

```json
{
  "name": "English Premier League 2016/17",
  "rounds": [
    {
      "name": "Matchday 1",
      "matches": [
        {
          "date": "2016-08-13",
          "team1": {
            "key": "hull",
            "name": "Hull City",
            "code": "HUL"
          },
          "team2": {
            "key": "leicester",
            "name": "Leicester City",
            "code": "LEI"
          },
          "score1": 2,
          "score2": 1
        },
        {
          "date": "2016-08-13",
          "team1": {
            "key": "burnley",
            "name": "Burnley",
            "code": "BUR"
          },
          "team2": {
            "key": "swansea",
            "name": "Swansea",
            "code": "SWA"
          },
          "score1": 0,
          "score2": 1
        }
      ]
    },
    {
      "name": "Matchday 2",
      "matches": [
        {
          "date": "2016-08-19",
          "team1": {
            "key": "manutd",
            "name": "Manchester United",
            "code": "MUN"
          },
          "team2": {
            "key": "southampton",
            "name": "Southampton",
            "code": "SOU"
          },
          "score1": 2,
          "score2": 0
        },
        {
          "date": "2016-08-20",
          "team1": {
            "key": "stoke",
            "name": "Stoke City",
            "code": "STK"
          },
          "team2": {
            "key": "mancity",
            "name": "Manchester City",
            "code": "MCI"
          },
          "score1": 1,
          "score2": 4
        }
      ]
    }
  ]
}
```

would convert to:

| name | rounds->name | rounds->matches->date | rounds->matches->team1->key | rounds->matches->team1->name | rounds->matches->team1->code | rounds->matches->team2->key | rounds->matches->team2->name | rounds->matches->team2->code | rounds->matches->score1 | rounds->matches->score2 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| English Premier League 2016/17 | Matchday 1 | 2016-08-13 | hull | Hull City | HUL | leicester | Leicester City | LEI | 2 | 1 |
| English Premier League 2016/17 | Matchday 1 | 2016-08-13 | burnley | Burnley | BUR | swansea | Swansea | SWA | 0 | 1 |
| English Premier League 2016/17 | Matchday 2 | 2016-08-19 | manutd | Manchester United | MUN | southampton | Southampton | SOU | 2 | 0 |
| English Premier League 2016/17 | Matchday 2 | 2016-08-20 | stoke | Stoke City | STK | mancity | Manchester City | MCI | 1 | 4 |

### Running

To clone, build, and run:

    git clone git@github.com:robertjlooby/json-to-csv.git
    cd json-to-csv
    stack exec json-to-csv -- < path/to/file.json > path/to/file.csv

To install using homebrew:

    brew tap robertjlooby/homebrew
    brew install json-to-csv
    json-to-csv < path/to/file.json > path/to/file.csv

### Tests

    stack test [--file-watch]
