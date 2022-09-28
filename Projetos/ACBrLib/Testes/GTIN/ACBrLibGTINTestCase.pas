{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibGTINTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibCEPNome = 'ACBrLibGTIN';

type

  { TTestACBrGTINLib }

  TTestACBrGTINLib = class(TTestCase)
  published
    procedure Test_GTIN_Inicializar_Com_DiretorioInvalido;
    procedure Test_GTIN_Inicializar;
    procedure Test_GTIN_Inicializar_Ja_Inicializado;
    procedure Test_GTIN_Finalizar;
    procedure Test_GTIN_Finalizar_Ja_Finalizado;
    procedure Test_GTIN_Nome_Obtendo_LenBuffer;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_GTIN_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_GTIN_Versao;
    procedure Test_GTIN_ConfigLerValor;
    procedure Test_GTIN_ConfigGravarValor;

    procedure Test_GTIN_Consultar;

  end;

implementation

uses
  Dialogs, ACBrLibGTINStaticImportMT, ACBrLibGTINConsts, ACBrLibConsts, ACBrUtil.Strings;

end.
