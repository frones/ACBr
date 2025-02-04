<?php
/* {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Renato Rubinho                                  }
// {                                                                              }
// {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
// { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
// {                                                                              }
// {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
// { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
// { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
// { qualquer versão posterior.                                                   }
// {                                                                              }
// {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
// { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
// { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
// {                                                                              }
// {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
// { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
// { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
// { Você também pode obter uma copia da licença em:                              }
// { http://www.opensource.org/licenses/lgpl-license.php                          }
// {                                                                              }
// { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
// {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
// {******************************************************************************}
*/
header('Content-Type: application/json; charset=UTF-8');

include 'ACBrGTINMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrGTIN";
$metodo = $_POST['metodo'];

if (ValidaFFI() != 0)
    exit;

$dllPath = CarregaDll(__DIR__, $nomeLib);

if ($dllPath == -10)
    exit;

$importsPath = CarregaImports(__DIR__, $nomeLib, 'MT');

if ($importsPath == -10)
    exit;

$iniPath = CarregaIniPath(__DIR__, $nomeLib);

$processo = "file_get_contents";
$ffi = CarregaContents($importsPath, $dllPath);
$handle = FFI::new("uintptr_t");

try {
    $resultado = "";
    $processo = "Inicializar";

    $processo = "GTIN_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "GTIN_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "Principal", "LogPath", $_POST['LogPath']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Principal", "LogNivel", $_POST['LogNivel']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "GTIN", "ExibirErroSchema", $_POST['exibirErroSchema']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "FormatoAlerta", $_POST['formatoAlerta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "SalvarGer", $_POST['SalvarGer']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "GTIN", "PathSalvar", $_POST['pathSalvar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "PathSchemas", $_POST['pathSchemas']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "GTIN", "SSLType", $_POST['SSLType']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "Timeout", $_POST['timeout']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "Ambiente", $_POST['ambiente']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "Visualizar", $_POST['visualizar']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "SalvarWS", $_POST['SalvarWS']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "SalvarArq", $_POST['SalvarArq']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "AdicionarLiteral", $_POST['AdicionarLiteral']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "GTIN", "PathGTIN", $_POST['PathGTIN']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "Proxy", "Servidor", $_POST['proxyServidor']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Porta", $_POST['proxyPorta']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Usuario", $_POST['proxyUsuario']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "Proxy", "Senha", $_POST['proxySenha']) != 0) exit;

        if (ConfigGravarValor($handle, $ffi, "DFe", "UF", $_POST['UF']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLCryptLib", $_POST['SSLCryptLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLHttpLib", $_POST['SSLHttpLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "SSLXmlSignLib", $_POST['SSLXmlSignLib']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "ArquivoPFX", $_POST['ArquivoPFX']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "DadosPFX", $_POST['DadosPFX']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "Senha", $_POST['senhaCertificado']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "DFe", "NumeroSerie", $_POST['NumeroSerie']) != 0) exit;

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "GTIN_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "Principal", "LogPath", $LogPath) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Principal", "LogNivel", $LogNivel) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "GTIN", "ExibirErroSchema", $exibirErroSchema) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "FormatoAlerta", $formatoAlerta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "SalvarGer", $SalvarGer) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "GTIN", "PathSalvar", $pathSalvar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "PathSchemas", $pathSchemas) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "GTIN", "SSLType", $SSLType) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "Timeout", $timeout) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "Ambiente", $ambiente) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "Visualizar", $visualizar) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "SalvarWS", $SalvarWS) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "SalvarArq", $SalvarArq) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "AdicionarLiteral", $AdicionarLiteral) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "GTIN", "PathGTIN", $PathGTIN) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "Proxy", "Servidor", $proxyServidor) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Porta", $proxyPorta) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Usuario", $proxyUsuario) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "Proxy", "Senha", $proxySenha) != 0) exit;

        if (ConfigLerValor($handle, $ffi, "DFe", "UF", $UF) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLCryptLib", $SSLCryptLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLHttpLib", $SSLHttpLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "SSLXmlSignLib", $SSLXmlSignLib) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "ArquivoPFX", $ArquivoPFX) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "DadosPFX", $DadosPFX) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "Senha", $senhaCertificado) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "DFe", "NumeroSerie", $NumeroSerie) != 0) exit;

        $processo = $metodo . "/" . "responseData";
        $responseData = [
            'dados' => [
                'LogPath' => $LogPath ?? '',
                'LogNivel' => $LogNivel ?? '',

                'exibirErroSchema' => $exibirErroSchema ?? '',
                'formatoAlerta' => $formatoAlerta ?? '',
                'SalvarGer' => $SalvarGer ?? '',
                'pathSalvar' => $pathSalvar ?? '',
                'pathSchemas' => $pathSchemas ?? '',
                'SSLType' => $SSLType ?? '',
                'timeout' => $timeout ?? '',
                'ambiente' => $ambiente ?? '',
                'visualizar' => $visualizar ?? '',
                'SalvarWS' => $SalvarWS ?? '',
                'SalvarArq' => $SalvarArq ?? '',
                'AdicionarLiteral' => $AdicionarLiteral ?? '',
                'PathGTIN' => $PathGTIN ?? '',

                'proxyServidor' => $proxyServidor ?? '',
                'proxyPorta' => $proxyPorta ?? '',
                'proxyUsuario' => $proxyUsuario ?? '',
                'proxySenha' => $proxySenha ?? '',

                'UF' => $UF ?? '',
                'SSLCryptLib' => $SSLCryptLib ?? '',
                'SSLHttpLib' => $SSLHttpLib ?? '',
                'SSLXmlSignLib' => $SSLXmlSignLib ?? '',
                'ArquivoPFX' => $ArquivoPFX ?? '',
                'DadosPFX' => $DadosPFX ?? '',
                'senhaCertificado' => $senhaCertificado ?? '',
                'NumeroSerie' => $NumeroSerie ?? ''
            ]
        ];
    }

    if ($metodo == "OpenSSLInfo") {
        $processo = "GTIN_OpenSSLInfo";

        if (OpenSSLInfo($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "ObterCertificados") {
        $processo = "GTIN_ObterCertificados";

        if (ObterCertificados($handle, $ffi, $resultado) != 0) {
            exit;
        }
    }

    if ($metodo == "Consultar") {
        $processo = "GTIN_Consultar";

        if (Consultar($handle, $ffi, $_POST['AGTIN'], $resultado) != 0) {
            exit;
        }
    }

    if ($metodo != "carregarConfiguracoes") {
        $processo = "responseData";
        $responseData = [
            'mensagem' => $resultado
        ];
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

try {
    if ($processo != "GTIN_Inicializar") {
        $processo = "GTIN_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);
