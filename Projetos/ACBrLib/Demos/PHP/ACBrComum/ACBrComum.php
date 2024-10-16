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

function ValidaFFI()
{
    if (!extension_loaded('ffi')) {
        echo json_encode(["mensagem" => "A extensão FFI não está habilitada."]);
        return -10;
    }

    return 0;
}

function CarregaDll($dir, $nomeLib)
{
    if (strpos(PHP_OS, 'WIN') === false) {
        $prefixo = strtolower("lib" . $nomeLib);
        $extensao = ".so";
    } else {
        $prefixo = strtolower("lib" . $nomeLib);
        $prefixo = $nomeLib;
        $extensao = ".dll";
    }

    if (strpos(php_uname('m'), '64') === false)
        $arquitetura = "86";
    else
        $arquitetura = "64";

    $biblioteca = $prefixo . $arquitetura . $extensao;

    $dllPath = $dir . DIRECTORY_SEPARATOR;

    if (!file_exists($dllPath . $biblioteca))
    {
        $dllPath = $dir . DIRECTORY_SEPARATOR . "ACBrLib\\x" . $arquitetura . DIRECTORY_SEPARATOR;

        if (!file_exists($dllPath . $biblioteca)){
            echo json_encode(["mensagem" => "DLL não encontrada no caminho especificado: $dllPath . $biblioteca"]);
            return -10;
        }
    }

    $pathAtual = getenv('PATH');
    if (strpos($pathAtual, $dllPath) === false) {
        putenv("PATH=$dllPath;" . $pathAtual);
    }    
    
    return $dllPath . $biblioteca;
}

function CarregaImports($dir, $nomeLib, $modo)
{
    $importsPath = $dir . DIRECTORY_SEPARATOR . $nomeLib . $modo . '.h';

    if (!file_exists($importsPath)) {
        echo json_encode(["mensagem" => "Imports não encontrados no caminho especificado: $importsPath"]);
        return -10;
    }

    return $importsPath;
}

function CarregaIniPath($dir, $nomeLib)
{
    return $dir . DIRECTORY_SEPARATOR . $nomeLib . ".INI";
}

function CarregaContents($importsPath, $dllPath)
{
    $ffi = FFI::cdef(
        file_get_contents($importsPath),
        $dllPath
    );

    return $ffi;
}

function VerificaXmlOuIni($conteudo)
{
    // 0=Ini
    if (preg_match('/^\[[^\]]+\]/', $conteudo)) {
        return 0;
    }

    // 1=Xml
    if (preg_match('/^<\?xml|<\w+>/', $conteudo)) {
        return 1;
    }

    // -1=Nenhum
    return -1;
}

function formataDataAMD($dateString){
    if (preg_match('/^\d{2}[\/-]\d{2}[\/-]\d{4}$/', $dateString)) {
        $dateString = str_replace('-', '/', $dateString);
        
        $date = DateTime::createFromFormat('d/m/Y', $dateString);
        return $date->format('Y/m/d');
    } else {
        return str_replace('-', '/', $dateString);
    }
}

function strDateToDoubleDate($dateString) {
    $baseDate = new DateTime('1899/12/30');
    $inputDate = new DateTime(formataDataAMD($dateString));

    // Diferença de dias entre a base e a data fornecida
    $daysDiff = $inputDate->diff($baseDate)->days;

    // Retorna o valor em formato double
    return $daysDiff;
}

function strDateTimeToDoubleDateTime($dateString) {
    $baseDate = new DateTime('1899/12/30');
    $inputDate = new DateTime(formataDataAMD($dateString));

    // Diferença de dias entre a base e a data fornecida
    $daysDiff = $inputDate->diff($baseDate)->days;

    // Fração de um dia (horas, minutos, segundos)
    $fraction = ($inputDate->format('H') / 24) +
                ($inputDate->format('i') / 1440) +
                ($inputDate->format('s') / 86400);

    // Retorna o valor em formato double
    return $daysDiff + $fraction;
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
