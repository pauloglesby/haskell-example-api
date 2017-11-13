CREATE TABLE cars (
       id SERIAL PRIMARY KEY,
       owner_id INTEGER NOT NULL REFERENCES accounts(id),
       make VARCHAR NOT NULL,
       model VARCHAR NOT NULL,
       colour VARCHAR NOT NULL,
       fuel VARCHAR NOT NULL,
       range INTEGER NOT NULL,
       created_at TIMESTAMP WITH TIME ZONE DEFAULT (NOW() AT TIME ZONE 'UTC'),
       updated_at TIMESTAMP WITH TIME ZONE DEFAULT (NOW() AT TIME ZONE 'UTC')
);
