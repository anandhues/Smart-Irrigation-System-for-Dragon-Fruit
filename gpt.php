<?php
$servername = "127.0.0.1";
$username = "root";
$password = "";
$dbname = "dragon_ml";

$temperature = $_GET['temperature'];
$humidity = $_GET['humidity'];
$soil_moisture = $_GET['soil_moisture'];

// Create connection
$conn = new mysqli($servername, $username, $password, $dbname);

// Check connection
if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

$sql = "INSERT INTO dht (temp, humidity, soil_moist)
        VALUES ('$temperature', '$humidity', '$soil_moisture')";

if ($conn->query($sql) === TRUE) {
    echo "Sensor data inserted successfully";
} else {
    echo "Error: " . $sql . "<br>" . $conn->error;
}

$conn->close();
?>
