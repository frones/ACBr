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

include 'ACBrCEPMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrCEP";
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

    $processo = "CEP_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "CEP_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "CEP", "Usuario", $_POST['usuario']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CEP", "Senha", $_POST['senha']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CEP", "ChaveAcesso", $_POST['chaveacesso']) != 0) exit;
        if (ConfigGravarValor($handle, $ffi, "CEP", "WebService", $_POST['webservice']) != 0) exit;

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "CEP_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "CEP", "Usuario", $usuario) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CEP", "Senha", $senha) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CEP", "ChaveAcesso", $chaveacesso) != 0) exit;
        if (ConfigLerValor($handle, $ffi, "CEP", "WebService", $webservice) != 0) exit;

        $processo = $metodo . "/" . "responseData";
        $responseData = [
            'retorno' => $resultado,
            'dados' => [
                'usuario' => $usuario ?? '',
                'senha' => $senha ?? '',
                'chaveacesso' => $chaveacesso ?? '',
                'webservice' => $webservice ?? '0'
            ]
        ];
    }

    if (($metodo == "BuscarPorCEP") || ($metodo == "BuscarPorLogradouro")) {
        $processo = $metodo . "/" . "CEP_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "CEP", "WebService", $_POST['webservice']) != 0) exit;

        $iniContent = "";

        if ($metodo == "BuscarPorCEP")
            $resultado = BuscarPorCEP($handle, $ffi, $_POST['cepcons'], $iniContent);
        else
            $resultado = BuscarPorLogradouro($handle, $ffi, $_POST['cidadecons'], $_POST['tipocons'], $_POST['logradourocons'], $_POST['ufcons'], $_POST['bairrocons'], $iniContent);

        if ($resultado != 0)
            exit;

        $parsedIni = parseIniToStr($iniContent);

        $processo = "responseData";
        $secao = "Endereco1";

        if (isset($parsedIni[$secao])) {
            $responseData = [
                'retorno' => $resultado,
                'mensagem' => $iniContent,
                'dados' => [
                    'bairro' => $parsedIni[$secao]['Bairro'],
                    'cep' => $parsedIni[$secao]['CEP'] ?? '',
                    'complemento' => $parsedIni[$secao]['Complemento'],
                    'ibgemunicipio' => $parsedIni[$secao]['IBGE_Municipio'] ?? '',
                    'ibgeuf' => $parsedIni[$secao]['IBGE_UF'] ?? '',
                    'logradouro' => $parsedIni[$secao]['Logradouro'],
                    'municipio' => $parsedIni[$secao]['Municipio'],
                    'tipologradouro' => $parsedIni[$secao]['Tipo_Logradouro'] ?? '',
                    'UF' => $parsedIni[$secao]['UF'] ?? ''
                ]
            ];
        } else {
            $responseData = [
                'retorno' => $resultado,
                'mensagem' => $iniContent,
                'dados' => ''
            ];
        }
    }

    if (($metodo != "carregarConfiguracoes") && ($metodo != "BuscarPorCEP") && ($metodo != "BuscarPorLogradouro")) {
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
    if ($processo != "CEP_Inicializar") {
        $processo = "CEP_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);
