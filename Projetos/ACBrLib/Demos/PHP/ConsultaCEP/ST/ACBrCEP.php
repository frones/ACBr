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
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÝFICA. Consulte a Licença Pública Geral Menor}
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

function ValidaFFI()
{
    if (!extension_loaded('ffi')) {
        echo json_encode(["mensagem" => "A extensão FFI não está habilitada."]);
        return -10;
    }

    return 0;
}

function CarregaDll()
{
    if (strpos(PHP_OS, 'WIN') === false) {
        $prefixo = "libacbrcep";
        $extensao = ".so";
    } else {
        $prefixo = "ACBrCEP";
        $extensao = ".dll";
    }

    if (strpos(php_uname('m'), '64') === false)
        $arquitetura = "86";
    else
        $arquitetura = "64";

    $biblioteca = $prefixo . $arquitetura . $extensao;

    $dllPath = __DIR__ . DIRECTORY_SEPARATOR . $biblioteca;

    if (file_exists($dllPath))
        return $dllPath;

    $dllPath = __DIR__ . DIRECTORY_SEPARATOR . "ACBrLib\\x" . $arquitetura . DIRECTORY_SEPARATOR . $biblioteca;

    if (file_exists($dllPath))
        return $dllPath;
    else {
        echo json_encode(["mensagem" => "DLL não encontrada no caminho especificado: $dllPath"]);
        return -10;
    }

    return $dllPath;
}

function CarregaImports()
{
    $importsPath = __DIR__ . DIRECTORY_SEPARATOR . 'ACBrCEP.h';

    if (!file_exists($importsPath)) {
        echo json_encode(["mensagem" => "Imports não encontrados no caminho especificado: $importsPath"]);
        return -10;
    }

    return $importsPath;
}

function CarregaIniPath()
{
    return __DIR__ . DIRECTORY_SEPARATOR . "ACBrCEP.INI";
}

function CarregaContents($importsPath, $dllPath)
{
    $ffi = FFI::cdef(
        file_get_contents($importsPath),
        $dllPath
    );

    return $ffi;
}

function Inicializar($ffi, $iniPath)
{
    $retorno = $ffi->CEP_Inicializar($iniPath, "");
    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao inicializar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function Finalizar($ffi)
{
    $retorno = $ffi->CEP_Finalizar();

    if ($retorno !== 0) {
        echo json_encode(["mensagem" => "Falha ao finalizar a biblioteca ACBr. Código de erro: $retorno"]);
        return -10;
    }

    return 0;
}

function ConfigLerValor($ffi, $eSessao, $eChave, &$sValor)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->CEP_ConfigLerValor($eSessao, $eChave, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao ler valor na secao[$eSessao] e chave[$eChave]. ", 1) != 0)
            return -10;
    }

    $sValor = FFI::string($sResposta);
    return 0;
}

function ConfigGravarValor($ffi, $eSessao, $eChave, $value)
{
    $retorno = $ffi->CEP_ConfigGravarValor($eSessao, $eChave, $value);
    $sMensagem = FFI::new("char[535]");

    if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao gravar valores [$value] na secao[$eSessao] e chave[$eChave]. ") != 0)
        return -10;

    return 0;
}

function UltimoRetorno($ffi, $retornolib, &$sMensagem, $msgErro, $retMensagem = 0)
{
    if (($retornolib !== 0) || ($retMensagem == 1)) {
        $esTamanho = FFI::new("long");
        $esTamanho->cdata = 9048;
        $ffi->CEP_UltimoRetorno($sMensagem, FFI::addr($esTamanho));

        if ($retornolib !== 0) {
            $ultimoRetorno = FFI::string($sMensagem);
            $retorno = "$msgErro Código de erro: $retornolib. ";

            if ($ultimoRetorno != "") {
                $retorno = $retorno . "Último retorno: " . mb_convert_encoding($ultimoRetorno, "UTF-8", "ISO-8859-1");
            }

            echo json_encode(["mensagem" => $retorno]);
            return -10;
        }
    }

    return 0;
}

function BuscarPorCEP($ffi, $cep, &$iniContent)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->CEP_BuscarPorCEP($cep, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar o cep.", 1) != 0)
            return -10;
    }

    $iniContent = FFI::string($sResposta);
    return 0;
}

function BuscarPorLogradouro($ffi, $cidadecons, $tipocons, $logradourocons, $ufcons, $bairrocons, &$iniContent)
{
    $sResposta = FFI::new("char[9048]");
    $esTamanho = FFI::new("long");
    $esTamanho->cdata = 9048;
    $retorno = $ffi->CEP_BuscarPorLogradouro($cidadecons, $tipocons, $logradourocons, $ufcons, $bairrocons, $sResposta, FFI::addr($esTamanho));

    $sMensagem = FFI::new("char[535]");

    if ($retorno !== 0) {
        if (UltimoRetorno($ffi, $retorno, $sMensagem, "Erro ao consultar o cep.", 1) != 0)
            return -10;
    }

    $iniContent = FFI::string($sResposta);
    return 0;
}

function parseIniToStr($ini)
{
    $lines = explode("\r\n", $ini);
    $config = [];
    $section = null;

    foreach ($lines as $line) {
        $line = trim($line);

        if ($line === '' || $line[0] === ';') {
            continue;
        }

        if ($line[0] === '[' && $line[-1] === ']') {
            $section = substr($line, 1, -1);
            $config[$section] = [];
        } else {
            list($key, $value) = explode('=', $line, 2);
            $key = trim($key);
            $value = trim($value);

            if ($section) {
                $config[$section][$key] = $value;
            } else {
                $config[$key] = $value;
            }
        }
    }

    return $config;
}
