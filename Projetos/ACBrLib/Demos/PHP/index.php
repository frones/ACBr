<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Menu</title>
    <style>
        body {
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            height: 100vh;
            margin: 0;
            font-family: Arial, sans-serif;
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
        }
        nav ul li a:hover {
            background-color: #0056b3;
        }
        img {
            margin-bottom: 20px;
        }
    </style>
</head>
<body>
<a href="https://projetoacbr.com.br/"><img src="https://projetoacbr.com.br/wp-content/uploads/2024/06/ACBr_144_144.png" alt="ACBr Logo"></a>
    <nav>
        <ul>
            <li><a href="/Boleto/ACBrBoletoDemoMT.php">Boleto</a></li>
            <li><a href="/ConsultaCEP/ACBrCEPDemoMT.php">CEP</a></li>
            <li><a href="/ConsultaCNPJ/ACBrConsultaCNPJDemoMT.php">ConsultaCNPJ</a></li>
            <li><a href="/CTe/ACBrCTeDemoMT.php">CTe</a></li>
            <li><a href="/GTIN/ACBrGTINDemoMT.php">GTIN</a></li>
            <li><a href="/MDFe/ACBrMDFeDemoMT.php">MDFe</a></li>
            <li><a href="/NFe/ACBrNFeDemoMT.php">NFe</a></li>
            <li><a href="/NFSe/ACBrNFSeDemoMT.php">NFSe</a></li>
        </ul>
    </nav>
</body>
</html>