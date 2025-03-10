<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Menu</title>
    <link rel="icon" href="https://projetoacbr.com.br/wp-content/uploads/2020/02/favico-acbr.png" type="image/x-icon">
    <style>
        body {
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            height: 100vh;
            margin: 0;
            font-family: Arial, sans-serif;
            background-image: url('https://projetoacbr.com.br/wp-content/uploads/2025/02/background-php-e1740514304960.jpg');
            background-size: cover; 
            background-position: center; 
        }
        nav ul {
            list-style-type: none;
            padding: 0;
        }
        nav ul li {
            margin: 10px 0;
        }
        nav ul li a {
            display: inline-block;
            width: 200px;
            text-align: center;
            padding: 10px 20px;
            text-decoration: none;
            color: white;
            background-color: #007BFF;
            border-radius: 5px;
            transition: background-color 0.3s;
            background: #333c4e;
        }
        nav ul li a:hover {
            background-color: #f4ad24;
        }
        img {
            margin-bottom: 20px;
            width: 150px;
            height: 150px;
        }
    </style>
</head>
<body>
<a href="https://projetoacbr.com.br/"><img src="https://projetoacbr.com.br/wp-content/uploads/2020/02/favico-acbr.png" alt="ACBr Logo"></a>
    <nav>
        <ul>
            <li><a href="/Boleto/ACBrBoletoDemoMT.php">Boleto</a></li>
            <li><a href="/ConsultaCEP/ACBrCEPDemoMT.php">CEP</a></li>
            <li><a href="/ConsultaCNPJ/ACBrConsultaCNPJDemoMT.php">ConsultaCNPJ</a></li>
            <li><a href="/GTIN/ACBrGTINDemoMT.php">GTIN</a></li>
            <li><a href="/NFe/ACBrNFeDemoMT.php">NFe</a></li>
            <li><a href="/NFSe/ACBrNFSeDemoMT.php">NFSe</a></li>
        </ul>
    </nav>
</body>
</html>