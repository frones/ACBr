{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit pnfsSubsNfseResposta;

interface

uses
  SysUtils, Classes, Forms, Contnrs,
  pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type

 TMsgRetornoSubsCollection = class;
 TMsgRetornoSubsCollectionItem = class;
 TNotaSubstituidoraCollection = class;
 TNotaSubstituidoraCollectionItem = class;

 TretSubsNFSe = class(TObject)
  private
    FLeitor: TLeitor;
    FMsgRetorno: TMsgRetornoSubsCollection;
    FNotaSubstituidora: TNotaSubstituidoraCollection;
    FProvedor: TnfseProvedor;

    procedure SetMsgRetorno(const Value: TMsgRetornoSubsCollection);
    procedure SetNotaSubstituidora(const Value: TNotaSubstituidoraCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;
    function LerXml_ABRASF: Boolean;
    function LerXml_proISSDSF: Boolean;
    function LerXML_proEquiplano: Boolean;
    function LerXML_proInfisc: Boolean;
    function LerXML_proEL: Boolean;
	  function LerXml_proNFSeBrasil: Boolean;
    property Leitor: TLeitor                                 read FLeitor            write FLeitor;
    property MsgRetorno: TMsgRetornoSubsCollection           read FMsgRetorno        write SetMsgRetorno;
    property NotaSubstituidora: TNotaSubstituidoraCollection read FNotaSubstituidora write SetNotaSubstituidora;
    property Provedor: TnfseProvedor                         read FProvedor          write FProvedor;
  end;

 TMsgRetornoSubsCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TMsgRetornoSubsCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoSubsCollectionItem);
  public
    function Add: TMsgRetornoSubsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMsgRetornoSubsCollectionItem;
    property Items[Index: Integer]: TMsgRetornoSubsCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoSubsCollectionItem = class(TObject)
  private
    FCodigo: String;
    FMensagem: String;
    FCorrecao: String;
  public
    property Codigo: String   read FCodigo   write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property Correcao: String read FCorrecao write FCorrecao;
  end;

 TNotaSubstituidoraCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TNotaSubstituidoraCollectionItem;
    procedure SetItem(Index: Integer; Value: TNotaSubstituidoraCollectionItem);
  public
    function Add: TNotaSubstituidoraCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TNotaSubstituidoraCollectionItem;
    property Items[Index: Integer]: TNotaSubstituidoraCollectionItem read GetItem write SetItem; default;
  end;

 TNotaSubstituidoraCollectionItem = class(TObject)
  private
    FNumeroNota: String;
    FCodigoVerficacao: String;
    FInscricaoMunicipalPrestador: String;
  public
    property NumeroNota: String                  read FNumeroNota                  write FNumeroNota;
    property CodigoVerficacao: String            read FCodigoVerficacao            write FCodigoVerficacao;
    property InscricaoMunicipalPrestador: String read FInscricaoMunicipalPrestador write FInscricaoMunicipalPrestador;
  end;

implementation

{ TMsgRetornoSubsCollection }

function TMsgRetornoSubsCollection.Add: TMsgRetornoSubsCollectionItem;
begin
  Result := Self.New;
end;

function TMsgRetornoSubsCollection.GetItem(
  Index: Integer): TMsgRetornoSubsCollectionItem;
begin
  Result := TMsgRetornoSubsCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoSubsCollection.SetItem(Index: Integer;
  Value: TMsgRetornoSubsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TMsgRetornoSubsCollection.New: TMsgRetornoSubsCollectionItem;
begin
  Result := TMsgRetornoSubsCollectionItem.Create;
  Self.Add(Result);
end;

{ TNotaSubstituidoraCollection }

function TNotaSubstituidoraCollection.Add: TNotaSubstituidoraCollectionItem;
begin
  Result := Self.New;
end;

function TNotaSubstituidoraCollection.GetItem(
  Index: Integer): TNotaSubstituidoraCollectionItem;
begin
  Result := TNotaSubstituidoraCollectionItem(inherited GetItem(Index));
end;

procedure TNotaSubstituidoraCollection.SetItem(Index: Integer;
  Value: TNotaSubstituidoraCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TNotaSubstituidoraCollection.New: TNotaSubstituidoraCollectionItem;
begin
  Result := TNotaSubstituidoraCollectionItem.Create;
  Self.Add(Result);
end;

{ TretSubsNFSe }

constructor TretSubsNFSe.Create;
begin
  inherited Create;
  FLeitor  := TLeitor.Create;
  FMsgRetorno := TMsgRetornoSubsCollection.Create;
  FNotaSubstituidora := TNotaSubstituidoraCollection.Create;
end;

destructor TretSubsNFSe.Destroy;
begin
  FLeitor.Free;
  FMsgRetorno.Free;
  FNotaSubstituidora.Free;
  inherited;
end;

procedure TretSubsNFSe.SetMsgRetorno(const Value: TMsgRetornoSubsCollection);
begin
  FMsgRetorno := Value;
end;

procedure TretSubsNFSe.SetNotaSubstituidora(const Value: TNotaSubstituidoraCollection);
begin
  FNotaSubstituidora := Value;
end;

function TretSubsNFSe.LerXml: Boolean;
begin
  if Provedor = proISSCuritiba then
    Leitor.Arquivo := RemoverNameSpace(Leitor.Arquivo)
  else
    Leitor.Arquivo := RemoverNameSpace(RetirarPrefixos(Leitor.Arquivo, Provedor));

  Leitor.Grupo   := Leitor.Arquivo;

 case Provedor of
   proISSDSF:     Result := LerXml_proISSDSF;
   proEquiplano:  Result := LerXML_proEquiplano;
   proInfisc,
   proInfiscv11:  Result := LerXml_proInfisc;
   proEL:         Result := LerXML_proEL;
   proNFSeBrasil: Result := LerXml_proNFSeBrasil;
 else
   Result := LerXml_ABRASF;
 end;
end;

function TretSubsNFSe.LerXml_ABRASF: Boolean;
var
  i: Integer;
begin
  Result := True;

  try
    if (leitor.rExtrai(1, 'SubstituirNfseResposta') <> '') then
    begin
      if (leitor.rExtrai(2, 'RetSubstituicao') <> '') then
      begin
        if (leitor.rExtrai(2, 'NfseSubstituidora') <> '') then
        begin
          // contem a nova NFS-e.
          // Falta implementar
        end;
      end;

      if (leitor.rExtrai(2, 'ListaMensagemRetorno') <> '') then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'MensagemRetorno', '', i + 1) <> '' do
        begin
          FMsgRetorno.New;
          FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
          inc(i);
        end;
      end;

      if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') then
      begin
        FMsgRetorno.New;
        FMsgRetorno[0].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        FMsgRetorno[0].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        FMsgRetorno[0].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
      end;

    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
    begin
      FMsgRetorno.New;
      FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
      FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
      FMsgRetorno[i].FCorrecao := '';

      inc(i);
    end;

  except
    Result := False;
  end;
end;

function TretSubsNFSe.LerXml_proISSDSF: Boolean; //falta homologar
begin
  Result := False;
end;

function TretSubsNFSe.LerXML_proEquiplano: Boolean;
begin
  Result := False;
end;

function TretSubsNFSe.LerXML_proInfisc: Boolean;
begin
  Result := False;
end;

function TretSubsNFSe.LerXML_proEL: Boolean;
begin
  Result := False;
end;

function TretSubsNFSe.LerXml_proNFSeBrasil: Boolean;
begin
  Result := False;
end;

end.

