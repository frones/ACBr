{******************************************************************************}
{ Projeto: Componente ACBrNF3e                                                 }
{  Nota Fiscal de Energia Eletrica Eletrônica - NF3e                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019                                        }
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

{*******************************************************************************
|* Historico
|*
|* 18/12/2019: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnRetEnvEventoNF3e;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnLeitor, pcnEventoNF3e, pcnSignature;

type

  TRetInfEventoCollectionItem = class(TObject)
  private
    FRetInfEvento: TRetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;

    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TRetInfEventoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfEventoCollectionItem);
  public
    function New: TRetInfEventoCollectionItem;
    property Items[Index: Integer]: TRetInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TRetEventoNF3e = class(TObject)
  private
    FidLote: Integer;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FLeitor: TLeitor;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: String;
    FretEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FXML: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property idLote: Integer                    read FidLote    write FidLote;
    property Leitor: TLeitor                    read FLeitor    write FLeitor;
    property versao: String                     read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb     write FtpAmb;
    property verAplic: String                   read FverAplic  write FverAplic;
    property cOrgao: Integer                    read FcOrgao    write FcOrgao;
    property cStat: Integer                     read FcStat     write FcStat;
    property xMotivo: String                    read FxMotivo   write FxMotivo;
    property InfEvento: TInfEvento              read FInfEvento write FInfEvento;
    property signature: Tsignature              read Fsignature write Fsignature;
    property retEvento: TRetInfEventoCollection read FretEvento write FretEvento;
    property XML: AnsiString                    read FXML       write FXML;
  end;


implementation

uses
  pcnConversaoNF3e;

{ TRetInfEventoCollection }

function TRetInfEventoCollection.GetItem(Index: Integer): TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem(inherited GetItem(Index));
end;

procedure TRetInfEventoCollection.SetItem(Index: Integer;
  Value: TRetInfEventoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetInfEventoCollection.New: TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEventoCollectionItem }

constructor TRetInfEventoCollectionItem.Create;
begin
  inherited Create;

  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TRetInfEventoCollectionItem.Destroy;
begin
  FRetInfEvento.Free;

  inherited;
end;

{ TRetEventoNF3e }

constructor TRetEventoNF3e.Create;
begin
  inherited Create;

  FLeitor := TLeitor.Create;
  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoNF3e.Destroy;
begin
  FLeitor.Free;
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

function TRetEventoNF3e.LerXml: Boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  Result := False;
  i:=0;
  try
    if (Leitor.rExtrai(1, 'evento') <> '') then
    begin
      if Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' then
      begin
        infEvento.ID           := Leitor.rAtributo('Id');
        InfEvento.cOrgao       := Leitor.rCampo(tcInt, 'cOrgao');
        infEvento.tpAmb        := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        infEvento.CNPJ         := Leitor.rCampo(tcStr, 'CNPJ');
        infEvento.chNF3e        := Leitor.rCampo(tcStr, 'chNF3e');
        infEvento.dhEvento     := Leitor.rCampo(tcDatHor, 'dhEvento');
        infEvento.tpEvento     := StrToTpEventoNF3e(ok,Leitor.rCampo(tcStr, 'tpEvento'));
        infEvento.nSeqEvento   := Leitor.rCampo(tcInt, 'nSeqEvento');
        infEvento.VersaoEvento := Leitor.rCampo(tcDe2, 'verEvento');

        if Leitor.rExtrai(3, 'detEvento', '', i + 1) <> '' then
        begin
          infEvento.DetEvento.descEvento := Leitor.rCampo(tcStr, 'descEvento');
          infEvento.DetEvento.xCorrecao  := Leitor.rCampo(tcStr, 'xCorrecao');
          infEvento.DetEvento.xCondUso   := Leitor.rCampo(tcStr, 'xCondUso');
          infEvento.DetEvento.nProt      := Leitor.rCampo(tcStr, 'nProt');
          infEvento.DetEvento.xJust      := Leitor.rCampo(tcStr, 'xJust');

          InfEvento.detEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');
          infEvento.detEvento.tpAutor     := StrToTipoAutor(ok, Leitor.rCampo(tcStr, 'tpAutor'));
          infEvento.detEvento.verAplic    := Leitor.rCampo(tcStr, 'verAplic');
          infEvento.detEvento.dhEmi       := Leitor.rCampo(tcDatHor, 'dhEmi');
          infEvento.detEvento.tpNF        := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
          infEvento.detEvento.IE          := Leitor.rCampo(tcStr, 'IE');

//           infEvento.detEvento.vNF         := Leitor.rCampo(tcDe2, 'vNF');
//           infEvento.detEvento.vICMS       := Leitor.rCampo(tcDe2, 'vICMS');
//           infEvento.detEvento.vST         := Leitor.rCampo(tcDe2, 'vST');

          if Leitor.rExtrai(4, 'dest', '', i + 1) <> '' then
          begin
            infEvento.detEvento.dest.UF            := Leitor.rCampo(tcStr, 'UF');
            infEvento.detEvento.dest.CNPJCPF       := Leitor.rCampoCNPJCPF;
            infEvento.detEvento.dest.idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');
            infEvento.detEvento.dest.IE            := Leitor.rCampo(tcStr, 'IE');

            infEvento.detEvento.vNF   := Leitor.rCampo(tcDe2, 'vNF');
            infEvento.detEvento.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
            infEvento.detEvento.vST   := Leitor.rCampo(tcDe2, 'vST');
          end;
        end;
      end;

      if Leitor.rExtrai(2, 'Signature', '', i + 1) <> '' then
      begin
        signature.URI             := Leitor.rAtributo('Reference URI=');
        signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
        signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
        signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
      end;

      Result := True;
    end;

    if (Leitor.rExtrai(1, 'retEnvEvento') <> '') or
       (Leitor.rExtrai(1, 'retEvento') <> '') then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FidLote   := Leitor.rCampo(tcInt, 'idLote');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FcOrgao   := Leitor.rCampo(tcInt, 'cOrgao');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');

      i := 0;
      while Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' do
       begin
         FretEvento.New;

         FretEvento.Items[i].FRetInfEvento.XML := Leitor.Grupo;

         FretEvento.Items[i].FRetInfEvento.Id         := Leitor.rAtributo('Id');
         FretEvento.Items[i].FRetInfEvento.tpAmb      := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
         FretEvento.Items[i].FRetInfEvento.verAplic   := Leitor.rCampo(tcStr, 'verAplic');
         FretEvento.Items[i].FRetInfEvento.cOrgao     := Leitor.rCampo(tcInt, 'cOrgao');
         FretEvento.Items[i].FRetInfEvento.cStat      := Leitor.rCampo(tcInt, 'cStat');
         FretEvento.Items[i].FRetInfEvento.xMotivo    := Leitor.rCampo(tcStr, 'xMotivo');
         FretEvento.Items[i].FRetInfEvento.chNF3e      := Leitor.rCampo(tcStr, 'chNF3e');
         FretEvento.Items[i].FRetInfEvento.tpEvento   := StrToTpEventoNF3e(ok,Leitor.rCampo(tcStr, 'tpEvento'));
         FretEvento.Items[i].FRetInfEvento.xEvento    := Leitor.rCampo(tcStr, 'xEvento');
         FretEvento.Items[i].FRetInfEvento.nSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');
         FretEvento.Items[i].FRetInfEvento.CNPJDest   := Leitor.rCampo(tcStr, 'CNPJDest');

         if FretEvento.Items[i].FRetInfEvento.CNPJDest = '' then
           FretEvento.Items[i].FRetInfEvento.CNPJDest  := Leitor.rCampo(tcStr, 'CPFDest');

         FretEvento.Items[i].FRetInfEvento.emailDest   := Leitor.rCampo(tcStr, 'emailDest');
         FretEvento.Items[i].FRetInfEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');
         FretEvento.Items[i].FRetInfEvento.dhRegEvento := Leitor.rCampo(tcDatHor, 'dhRegEvento');
         FretEvento.Items[i].FRetInfEvento.nProt       := Leitor.rCampo(tcStr, 'nProt');

         j := 0;
         while  Leitor.rExtrai(3, 'chNF3ePend', '', j + 1) <> '' do
         begin
           FretEvento.Items[i].FRetInfEvento.chNF3ePend.New;

           FretEvento.Items[i].FRetInfEvento.chNF3ePend[j].ChavePend := Leitor.rCampo(tcStr, 'chNF3ePend');

           inc(j);
         end;

         inc(i);
       end;

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.
