{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAF_N;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO N1 - IDENTIFICAÇÃO DO DESENVOLVEDOR DO PAF-ECF:

  TRegistroN1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO N2 - IDENTIFICAÇÃO DO PAF-ECF

  { TRegistroN2 }

  TRegistroN2 = class
  private
    fLAUDO:  string;
    fNOME:   string;
    fVERSAO: string;
  public
    property LAUDO: string read fLAUDO write fLAUDO;
 //Número do Laudo de Análise Funcional
    property NOME: string read fNOME write fNOME;
//Nome do aplicativo indicado no Laudo de Análise Técnica
    property VERSAO: string read fVERSAO write fVERSAO;
//Versão atual do aplicativo indicado no Laudo de Análise Técnica
  end;

  /// REGISTRO TIPO N3 - RELAÇÃO DOS EXECUTÁVEIS E SEUS CÓDIGOS DE AUTENTICAÇÃO (MD5):

  { TRegistroN3 }

  TRegistroN3 = class
  private
    fMD5: string;
    fNOME_ARQUIVO: string;
  public
    property NOME_ARQUIVO : string read fNOME_ARQUIVO write fNOME_ARQUIVO;
    property MD5 : string read fMD5 write fMD5;
  end;

  /// REGISTRO TIPO N3 - Lista

  TRegistroN3List = class(TObjectList)
  private
    function GetItem(Index: integer): TRegistroN3;
    procedure SetItem(Index: integer; const Value: TRegistroN3);
  public
    function New: TRegistroN3;
    property Items[Index: integer]: TRegistroN3 read GetItem write SetItem; default;
  end;

  /// REGISTRO TIPO N9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroN9 = class(TRegistroX9)
  end;

implementation

(* TRegistroN3List *)

function TRegistroN3List.GetItem(Index: integer): TRegistroN3;
begin
  Result := TRegistroN3(inherited Items[Index]);
end;

function TRegistroN3List.New: TRegistroN3;
begin
  Result := TRegistroN3.Create;
  Add(Result);
end;

procedure TRegistroN3List.SetItem(Index: integer; const Value: TRegistroN3);
begin
  Put(Index, Value);
end;

end.

