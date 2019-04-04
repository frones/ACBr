{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{ ******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2015: Guilherme Costa
|*  - não estava sendo gerada a tag "tpProc"
****************************************************************************** }

{$I ACBr.inc}
unit pcesRetConsultaIdentEvt;

interface

uses
  SysUtils, Classes, Contnrs,
  ACBrUtil, pcnAuxiliar, pcnConversao, pcnLeitor,
  pcesCommon, pcesRetornoClass, pcesConversaoeSocial,
  pcesS5001, pcesS5011;

type
  TRetIdentEvtsCollectionItem = class(TObject)
  private
    FIDEvento: string;
    FnrRec: string;
  public
    property Id: string read FIDEvento write FIDEvento;
    property nrRec: string read FnrRec write FnrRec;
  end;

  TRetIdentEvtsCollection = class(TObjectList)
  private
    FqtdeTotEvtsConsulta: integer;
    FdhUltimoEvtRetornado: TDateTime;
    function GetItem(Index: Integer): TRetIdentEvtsCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetIdentEvtsCollectionItem);
  public
    function Add: TRetIdentEvtsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetIdentEvtsCollectionItem;
    property Items[Index: Integer]: TRetIdentEvtsCollectionItem read GetItem write SetItem; default;
    property qtdeTotEvtsConsulta: integer read FqtdeTotEvtsConsulta write FqtdeTotEvtsConsulta;
    property dhUltimoEvtRetornado: TDateTime read FdhUltimoEvtRetornado write FdhUltimoEvtRetornado;
  end;

  TRetConsultaIdentEvt = class(TObject)
  private
    FLeitor: TLeitor;
    FStatus: TStatus;
    FRetIdentEvts: TRetIdentEvtsCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Status: TStatus read FStatus write FStatus;
    property RetIdentEvts: TRetIdentEvtsCollection read FRetIdentEvts write FRetIdentEvts;
  end;

implementation

{ TRetIdentEvtsCollection }

function TRetIdentEvtsCollection.Add: TRetIdentEvtsCollectionItem;
begin
  Result := Self.New;
end;

function TRetIdentEvtsCollection.GetItem(Index: Integer) : TRetIdentEvtsCollectionItem;
begin
  Result := TRetIdentEvtsCollectionItem(Inherited GetItem(Index));
end;

procedure TRetIdentEvtsCollection.SetItem(Index: Integer;
  Value: TRetIdentEvtsCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

function TRetIdentEvtsCollection.New: TRetIdentEvtsCollectionItem;
begin
  Result := TRetIdentEvtsCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetConsultaIdentEvt }

constructor TRetConsultaIdentEvt.Create;
begin
  inherited Create;
  FLeitor       := TLeitor.Create;
  FStatus       := TStatus.Create;
  FRetIdentEvts := TRetIdentEvtsCollection.Create;
end;

destructor TRetConsultaIdentEvt.Destroy;
begin
  FLeitor.Free;
  FStatus.Free;
  FRetIdentEvts.Free;

  inherited;
end;

function TRetConsultaIdentEvt.LerXml: boolean;
var
//  ok: boolean;
  i{, j, k}: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if Leitor.rExtrai(1, 'retornoConsultaIdentificadoresEvts') <> '' then
    begin

      if Leitor.rExtrai(2, 'status') <> '' then
      begin
        Status.cdResposta := Leitor.rCampo(tcInt, 'cdResposta');
        Status.descResposta := Leitor.rCampo(tcStr, 'descResposta');
      end;

      if Leitor.rExtrai(2, 'retornoIdentificadoresEvts') <> '' then
      begin
        RetIdentEvts.qtdeTotEvtsConsulta := Leitor.rCampo(tcInt, 'qtdeTotEvtsConsulta');
        RetIdentEvts.dhUltimoEvtRetornado := Leitor.rCampo(tcDat, 'dhUltimoEvtRetornado');

        i := 0;
        while Leitor.rExtrai(3, 'identificadoresEvts', '', i + 1) <> '' do
        begin
          RetIdentEvts.New;
          RetIdentEvts.Items[i].Id := FLeitor.rCampo(tcStr, 'id');
          RetIdentEvts.Items[i].nrRec := Leitor.rCampo(tcStr, 'nrRec');

          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
