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

include 'ACBrIBGEMT.php';
include '../../ACBrComum/ACBrComum.php';

$nomeLib = "ACBrIBGE";
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

    $processo = "IBGE_Inicializar";
    if (Inicializar($handle, $ffi, $iniPath) != 0)
        exit;

    if ($metodo == "salvarConfiguracoes") {
        $processo = $metodo . "/" . "IBGE_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "IBGE", "IgnorarCaixaEAcentos", $_POST['IgnorarCaixaEAcentos']) != 0) exit;

        $resultado = "Configurações salvas com sucesso.";
    }

    if ($metodo == "carregarConfiguracoes") {
        $processo = $metodo . "/" . "IBGE_ConfigLer";

        if (ConfigLerValor($handle, $ffi, "IBGE", "IgnorarCaixaEAcentos", $IgnorarCaixaEAcentos) != 0) exit;

        $processo = $metodo . "/" . "responseData";
        $responseData = [
            'retorno' => $resultado,
            'dados' => [
                'IgnorarCaixaEAcentos' => $IgnorarCaixaEAcentos ?? '0'
            ]
        ];
    }

    if (($metodo == "BuscarPorCodigo") || ($metodo == "BuscarPorNome")) {
        $processo = $metodo . "/" . "IBGE_ConfigGravarValor";

        if (ConfigGravarValor($handle, $ffi, "IBGE", "IgnorarCaixaEAcentos", $_POST['IgnorarCaixaEAcentos']) != 0) exit;

        $iniContent = "";

        if ($metodo == "BuscarPorCodigo")
            $resultado = BuscarPorCodigo($handle, $ffi, $_POST['ACodMun'], $iniContent);
        else
            $resultado = BuscarPorNome($handle, $ffi, $_POST['cidadecons'], $_POST['ufcons'], $iniContent);

        if ($resultado != 0)
            exit;

        $parsedIni = parseIniToStr($iniContent);

        $processo = "responseData";
        $secao = "Endereco1";

        if (isset($parsedIni[$secao])) {
            $responseData = [
                'retorno' => $resultado,
                'mensagem' => $iniContent
            ];
        } else {
            $responseData = [
                'retorno' => $resultado,
                'mensagem' => $iniContent,
                'dados' => ''
            ];
        }
    }

    if (($metodo != "carregarConfiguracoes") && ($metodo != "BuscarPorCodigo") && ($metodo != "BuscarPorNome")) {
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
    if ($processo != "IBGE_Inicializar") {
        $processo = "IBGE_Finalizar";
        if (Finalizar($handle, $ffi) != 0)
            exit;
    }
} catch (Exception $e) {
    $erro = $e->getMessage();
    echo json_encode(["mensagem" => "Exceção[$processo]: $erro"]);
    exit;
}

echo json_encode($responseData);
