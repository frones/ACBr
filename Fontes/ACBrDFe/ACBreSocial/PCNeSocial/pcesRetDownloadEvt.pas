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
unit pcesRetDownloadEvt;

interface

uses
  SysUtils, Classes,
  ACBrUtil, pcnAuxiliar, pcnConversao, pcnLeitor,
  pcesCommon, pcesRetornoClass, pcesConversaoeSocial,
  pcesS5001, pcesS5011;

type
  TArquivoCollection = class;
  TArquivoCollectionItem = class;
  TRetDownloadEvt = class;

  TArquivoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TArquivoCollectionItem;
    procedure SetItem(Index: Integer; Value: TArquivoCollectionItem);
  public
    constructor create(AOwner: TRetDownloadEvt);

    function Add: TArquivoCollectionItem;
    property Items[Index: Integer]: TArquivoCollectionItem read GetItem write SetItem; default;
  end;

  TArquivoCollectionItem = class(TCollectionItem)
  private
    FStatus: TStatus;
    FId: string;
    FnrRec: string;
    FXML: string;
  public
    constructor create; reintroduce;
    destructor Destroy; override;

    property Status: TStatus read FStatus write FStatus;
    property Id: string read FId write FId;
    property nrRec: string read FnrRec write FnrRec;
    property XML: string read FXML write FXML;
  end;

  TRetDownloadEvt = class(TPersistent)
  private
    FLeitor: TLeitor;
    FStatus: TStatus;
    FArquivo: TArquivoCollection;

    procedure SetArquivo(const Value: TArquivoCollection);
  public
    constructor create;
    destructor Destroy; override;

    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Status: TStatus read FStatus write FStatus;
    property Arquivo: TArquivoCollection read FArquivo write SetArquivo;
  end;

implementation

{ TRetIdentEvtsCollection }

function TArquivoCollection.Add: TArquivoCollectionItem;
begin
  Result := TArquivoCollectionItem(inherited Add());
  Result.create;
end;

constructor TArquivoCollection.create(AOwner: TRetDownloadEvt);
begin
  inherited create(TArquivoCollectionItem);
end;

function TArquivoCollection.GetItem(Index: Integer) : TArquivoCollectionItem;
begin
  Result := TArquivoCollectionItem(Inherited GetItem(Index));
end;

procedure TArquivoCollection.SetItem(Index: Integer;
  Value: TArquivoCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TRetIdentEvtsCollectionItem }

constructor TArquivoCollectionItem.create;
begin
  FStatus := TStatus.Create;
end;

destructor TArquivoCollectionItem.Destroy;
begin
  FStatus.Free;

  inherited;
end;

{ TRetDownloadEvt }

constructor TRetDownloadEvt.create;
begin
  FLeitor := TLeitor.create;
  FStatus := TStatus.create;
  FArquivo := TArquivoCollection.create(Self);
end;

destructor TRetDownloadEvt.Destroy;
begin
  FLeitor.Free;
  FStatus.Free;
  FArquivo.Free;

  inherited;
end;

procedure TRetDownloadEvt.SetArquivo(const Value: TArquivoCollection);
begin
  FArquivo := Value;
end;

function TRetDownloadEvt.LerXml: boolean;
var
//  ok: boolean;
  i{, j, k}: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if Leitor.rExtrai(1, 'download') <> '' then
    begin

      if Leitor.rExtrai(2, 'status') <> '' then
      begin
        Status.cdResposta := Leitor.rCampo(tcInt, 'cdResposta');
        Status.descResposta := Leitor.rCampo(tcStr, 'descResposta');
      end;

      if Leitor.rExtrai(2, 'retornoSolicDownloadEvts') <> '' then
      begin
        if Leitor.rExtrai(3, 'arquivos') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'arquivo', '', i + 1) <> '' do
          begin
            Arquivo.Add;

            if Leitor.rExtrai(5, 'status') <> '' then
            begin
              Arquivo.Items[i].Status.cdResposta   := Leitor.rCampo(tcInt, 'cdResposta');
              Arquivo.Items[i].Status.descResposta := Leitor.rCampo(tcStr, 'descResposta');
            end;

            if Leitor.rExtrai(5, 'evt') <> '' then
            begin
              Arquivo.Items[i].Id  := FLeitor.rAtributo('Id=', 'evt');
              Arquivo.Items[i].XML := RetornarConteudoEntre(Leitor.Grupo, '>', '</evt');
            end;

            if Leitor.rExtrai(5, 'rec') <> '' then
            begin
              Arquivo.Items[i].nrRec := FLeitor.rAtributo('nrRec=', 'rec');
              Arquivo.Items[i].XML   := RetornarConteudoEntre(Leitor.Grupo, '>', '</rec');
            end;

            inc(i);
          end;
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
