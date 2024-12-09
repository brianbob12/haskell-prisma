import sqlite3

def create(c):
    c.execute("""CREATE TABLE Users (
        id integer,
        email text,
        name text,
        dob integer
    )""")

def main():
    conn = sqlite3.connect("db")
    c = conn.cursor()
    try:
        create(c)
        conn.commit()
    except:
        print("The database is already created")
    conn.close()

if __name__ == "__main__": 
    main()
