<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

//Inicia componente Acbr
$nfe = new ACBrNFe();
echo $nfe->Nome . " " . $nfe->Versao;