import requests

def get_cdse_token(username: str, password: str) -> str:
    url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
    data = {
        "client_id": "cdse-public",
        "username": username,
        "password": password,
        "grant_type": "password"
    }
    r = requests.post(url, data=data, timeout=30)
    r.raise_for_status()
    tok = r.json().get("access_token")
    if not tok:
        raise Exception("Failed to get access token: %s" % r.text)
    return tok

if __name__ == "__main__":
    username = input("CDSE username: ")
    password = input("CDSE password: ")
    token = get_cdse_token(username, password)
    print("Access token:", token)
