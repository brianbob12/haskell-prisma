import sqlite3
import os
import argparse


def create_database_from_sql(sql_file_parent_dir: str, db_path: str) -> None:
    """
    Creates a SQLite database from a SQL file containing table creation statements.

    Args:
        sql_file_parent_dir (str): Parent directory containing the SQL file
        db_path (str): Path where the SQLite database should be created
    """
    # Construct the full path to the SQL file
    sql_file_path = os.path.join(sql_file_parent_dir, "createTable.sql")

    # Read the SQL file
    try:
        with open(sql_file_path, "r") as sql_file:
            sql_script = sql_file.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"SQL file not found at {sql_file_path}")

    # Create and connect to the SQLite database
    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()

        # Execute the SQL script
        cursor.executescript(sql_script)

        # Commit the changes and close the connection
        conn.commit()
        conn.close()
        print(f"Database created successfully at {db_path}")

    except sqlite3.Error as e:
        raise sqlite3.Error(f"Error creating database: {str(e)}")


if __name__ == "__main__":
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Create SQLite database from SQL file")
    parser.add_argument("--sql-dir", help="Parent directory containing the SQL file")
    parser.add_argument(
        "--db-path", help="Path where the SQLite database should be created"
    )

    args = parser.parse_args()

    # Use command line arguments if provided, otherwise prompt for input
    sql_file_parent_dir = (
        args.sql_dir
        if args.sql_dir
        else input("Enter the parent directory containing the SQL file: ")
    )
    db_path = (
        args.db_path
        if args.db_path
        else input("Enter the path where the SQLite database should be created: ")
    )

    try:
        create_database_from_sql(sql_file_parent_dir, db_path)
    except (FileNotFoundError, sqlite3.Error) as e:
        print(f"Error: {str(e)}")
