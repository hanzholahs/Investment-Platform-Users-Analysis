CREATE DATABASE IF NOT EXISTS bibit_problem_test;

USE bibit_problem_test;

DROP TABLE records;
DROP TABLE customers;

CREATE TABLE IF NOT EXISTS customers (
	user_id INT (25) PRIMARY KEY,
	registration_import_datetime DATETIME NOT NULL,
	user_gender CHAR (50) NOT NULL,
	user_age INT (5) NOT NULL,
	user_occupation CHAR (50) NOT NULL,
	user_income_range CHAR (50) NOT NULL,
	referral_code_used CHAR (50) DEFAULT NULL,
	user_income_source CHAR (50) NOT NULL
);

CREATE TABLE IF NOT EXISTS records (
	user_id INT (25),
	record_date DATE NOT NULL,
    product CHAR (25) NOT NULL,
    invested_amount BIGINT NOT NULL,
    AUM BIGINT NOT NULL, 
    FOREIGN KEY (user_id) REFERENCES customers(user_id)
);

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Stockbit_Bibit_PST/dataset1.csv'
INTO TABLE customers
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Stockbit_Bibit_PST/dataset2.csv'
INTO TABLE records
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;
