CREATE TABLE accounts (
       id SERIAL PRIMARY KEY,
       name VARCHAR NOT NULL,
       post_code VARCHAR NOT NULL,
       email VARCHAR NOT NULL,
       created_at TIMESTAMP WITH TIME ZONE DEFAULT (NOW() AT TIME ZONE 'UTC'),
       updated_at TIMESTAMP WITH TIME ZONE DEFAULT (NOW() AT TIME ZONE 'UTC')
);

CREATE UNIQUE INDEX account_email ON accounts (email);
