use anyhow::{bail, Result};
use tokio::io::AsyncReadExt;

pub async fn read_headers(
    reader: &mut (impl AsyncReadExt + Unpin),
) -> Result<Vec<(String, String)>> {
    let mut data = vec![];
    while !data.ends_with("\r\n\r\n".as_bytes()) {
        let mut buf = vec![0; 1];
        let n = reader.read(&mut buf).await?;
        if n == 0 {
            bail!("UnexpectedEof");
        }

        data.push(buf[0]);
    }

    Ok(parse_headers(&String::from_utf8(data)?))
}

fn parse_headers(headers: &str) -> Vec<(String, String)> {
    headers
        .split("\r\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut parts = line.split(": ");
            let key = parts.next().unwrap().to_string();
            let value = parts.next().unwrap().to_string();

            (key, value)
        })
        .collect()
}

#[test]
fn test_parse_headers() {
    let cases = vec![
        (
            "Content-Length: 10\r\nContent-Type: application/json",
            vec![
                ("Content-Length".to_string(), "10".to_string()),
                ("Content-Type".to_string(), "application/json".to_string()),
            ],
        ),
        (
            "Content-Length: 10\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n",
            vec![
                ("Content-Length".to_string(), "10".to_string()),
                (
                    "Content-Type".to_string(),
                    "application/vscode-jsonrpc; charset=utf-8".to_string(),
                ),
            ],
        ),
    ];

    for (input, expected) in cases {
        assert_eq!(parse_headers(input), expected);
    }
}
