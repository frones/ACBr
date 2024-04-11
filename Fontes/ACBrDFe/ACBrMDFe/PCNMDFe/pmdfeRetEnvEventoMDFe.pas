{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

unit pmdfeRetEnvEventoMDFe;

interface

uses
  SysUtils, Classes,
  {$IFNDEF VER130}
   Variants,
  {$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnLeitor, pmdfeEventoMDFe, pcnSignature;

type
  TRetInfEventoCollectionItem = class;

  TRetInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfEventoCollectionItem);
  public
    function Add: TRetInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfEventoCollectionItem;
    property Items[Index: Integer]: TRetInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfEventoCollectionItem = class(TObject)
  private
    FRetInfEvento: TRetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TRetEventoMDFe = class(TObject)
  private
    FLeitor: TLeitor;
    FidLote: Int64;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: String;
    FretEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    FXML: String;
    Fsignature: Tsignature;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;

    property Leitor: TLeitor                    read FLeitor    write FLeitor;
    property idLote: Int64                      read FidLote    write FidLote;
    property versao: String                     read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb     write FtpAmb;
    property verAplic: String                   read FverAplic  write FverAplic;
    property cOrgao: Integer                    read FcOrgao    write FcOrgao;
    property cStat: Integer                     read FcStat     write FcStat;
    property xMotivo: String                    read FxMotivo   write FxMotivo;
    property InfEvento: TInfEvento              read FInfEvento write FInfEvento;
    property signature: Tsignature              read Fsignature write Fsignature;
    property retEvento: TRetInfEventoCollection read FretEvento write FretEvento;
    property XML: String                        read FXML       write FXML;
  end;

implementation

uses
  pmdfeMDFe,
  pmdfeConversaoMDFe;

{ TRetInfEventoCollection }

function TRetInfEventoCollection.Add: TRetInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfEventoCollection.GetItem(
  Index: Integer): TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TRetInfEventoCollection.SetItem(Index: Integer;
  Value: TRetInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
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

{ TRetEventoMDFe }

constructor TRetEventoMDFe.Create;
begin
  inherited Create;
  FLeitor    := TLeitor.Create;
  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoMDFe.Destroy;
begin
  FLeitor.Free;
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;
  inherited;
end;

function TRetEventoMDFe.LerXml: boolean;
var
  ok: boolean;
  i, j: Integer;
  sAux: string;
  ItemComp: TCompCollectionItem;
  ItemInfPrazo: TInfPrazoCollectionItem;
begin
  Result := False;

  try
    if (Leitor.rExtrai(1, 'eventoMDFe') <> '') then
    begin
      if Leitor.rExtrai(2, 'infEvento') <> '' then
      begin
        infEvento.Id         := Leitor.rAtributo('Id', 'infEvento');
        infEvento.cOrgao     := Leitor.rCampo(tcInt, 'cOrgao');
        infEvento.tpAmb      := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        infEvento.CNPJCPF    := Leitor.rCampoCNPJCPF; //Leitor.rCampo(tcStr, 'CNPJ');
        infEvento.chMDFe     := Leitor.rCampo(tcStr, 'chMDFe');
        infEvento.dhEvento   := Leitor.rCampo(tcDatHor, 'dhEvento');
        infEvento.tpEvento   := StrToTpEventoMDFe(ok,Leitor.rCampo(tcStr, 'tpEvento'));
        infEvento.nSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');

        if Leitor.rExtrai(3, 'detEvento') <> '' then
        begin
          infEvento.VersaoEvento         := Leitor.rAtributo('versaoEvento', 'detEvento');
          infEvento.detEvento.descEvento := Leitor.rCampo(tcStr, 'descEvento');
          infEvento.detEvento.nProt      := Leitor.rCampo(tcStr, 'nProt');
          infEvento.detEvento.dtEnc      := Leitor.rCampo(tcDat, 'dtEnc');
          infEvento.detEvento.cUF        := Leitor.rCampo(tcInt, 'cUF');
          infEvento.detEvento.cMun       := Leitor.rCampo(tcInt, 'cMun');
          infEvento.detEvento.xJust      := Leitor.rCampo(tcStr, 'xJust');
          infEvento.detEvento.xNome      := Leitor.rCampo(tcStr, 'xNome');
          infEvento.detEvento.CPF        := Leitor.rCampo(tcStr, 'CPF');

          sAux := Leitor.rCampo(tcStr, 'indEncPorTerceiro');

          if sAux = '1' then
            infEvento.detEvento.indEncPorTerceiro := tiSim;

          infEvento.detEvento.cMunCarrega := Leitor.rCampo(tcInt, 'cMunCarrega');
          infEvento.detEvento.xMunCarrega := Leitor.rCampo(tcStr, 'xMunCarrega');

          if Leitor.rExtrai(4, 'infViagens') <> '' then
          begin
            InfEvento.detEvento.infViagens.qtdViagens := Leitor.rCampo(tcInt, 'qtdViagens');
            InfEvento.detEvento.infViagens.nroViagem  := Leitor.rCampo(tcInt, 'nroViagem');
          end;

          // Carrega os dados da informação de Documentos
          i := 0;
          while Leitor.rExtrai(4, 'infDoc', '', i + 1) <> '' do
          begin
            with infEvento.detEvento.infDoc.New do
            begin
              cMunDescarga := Leitor.rCampo(tcInt, 'cMunDescarga');
              xMunDescarga := Leitor.rCampo(tcStr, 'xMunDescarga');
              chNFe        := Leitor.rCampo(tcStr, 'chNFe');
            end;

            inc(i);
          end;

          // Informações sobre o pagamento
          i := 0;
          while Leitor.rExtrai(4, 'infPag', '', i + 1) <> '' do
          begin
            with infEvento.detEvento.infPag.New do
            begin
              xNome         := Leitor.rCampo(tcStr, 'xNome');
              idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');
              CNPJCPF       := Leitor.rCampo(tcStr, 'CNPJ');

              if CNPJCPF = '' then
                CNPJCPF := Leitor.rCampo(tcStr, 'CPF');

              vContrato := Leitor.rCampo(tcDe2, 'vContrato');
              indPag    := StrToTIndPag(ok, Leitor.rCampo(tcStr, 'indPag'));
              vAdiant   := Leitor.rCampo(tcDe2, 'vAdiant');

              if Leitor.rCampo(tcStr, 'indAntecipaAdiant') <> '' then
                indAntecipaAdiant := StrToTIndicador(ok, Leitor.rCampo(tcStr, 'indAntecipaAdiant'));

              tpAntecip := StrTotpAntecip(ok, Leitor.rCampo(tcStr, 'tpAntecip'));

              j := 0;
              while Leitor.rExtrai(5, 'Comp', '', j + 1) <> '' do
              begin
                ItemComp := infEvento.detEvento.infPag[i].Comp.New;

                ItemComp.tpComp := StrToTComp(Ok, Leitor.rCampo(tcStr, 'tpComp'));
                ItemComp.vComp  := Leitor.rCampo(tcDe2, 'vComp');
                ItemComp.xComp  := Leitor.rCampo(tcStr, 'xComp');

                inc(j);
              end;

              // Informações do pagamento a prazo.
              // Obs: Informar somente se indPag for à Prazo
              if indPag = ipPrazo then
              begin
                j := 0;
                while Leitor.rExtrai(5, 'infPrazo', '', j + 1) <> '' do
                begin
                  ItemInfPrazo := infEvento.detEvento.infPag[i].infPrazo.New;

                  ItemInfPrazo.nParcela := Leitor.rCampo(tcInt, 'nParcela');
                  ItemInfPrazo.dVenc    := Leitor.rCampo(tcDat, 'dVenc');
                  ItemInfPrazo.vParcela := Leitor.rCampo(tcDe2, 'vParcela');

                  inc(j);
                end;
              end;

              if Leitor.rExtrai(5, 'infBanc') <> '' then
              begin
                infBanc.PIX := Leitor.rCampo(tcStr, 'PIX');

                if infBanc.PIX = '' then
                begin
                  infBanc.CNPJIPEF := Leitor.rCampo(tcStr, 'CNPJIPEF');

                  if infBanc.CNPJIPEF = '' then
                  begin
                    infBanc.codBanco   := Leitor.rCampo(tcStr, 'codBanco');
                    infBanc.codAgencia := Leitor.rCampo(tcStr, 'codAgencia');
                  end;
                end;
              end;
            end;

            inc(i);
          end;
        end;
      end;

      if Leitor.rExtrai(2, 'Signature') <> '' then
      begin
        signature.URI             := Leitor.rAtributo('Reference URI=');
        signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
        signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
        signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
      end;

      Result := True;
    end;

    if (Leitor.rExtrai(1, 'retEnvEvento') <> '') or
       (Leitor.rExtrai(1, 'retEventoMDFe') <> '') then
    begin
      i := 0;
      Fversao := Leitor.rAtributo('versao');

      while Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' do
      begin
        FretEvento.New;

        FretEvento.Items[i].FRetInfEvento.XML := Leitor.Grupo;

        FretEvento.Items[i].FRetInfEvento.Id       := Leitor.rAtributo('Id', 'infEvento');
        FretEvento.Items[i].FRetInfEvento.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        FretEvento.Items[i].FRetInfEvento.verAplic := Leitor.rCampo(tcStr, 'verAplic');
        FretEvento.Items[i].FRetInfEvento.cOrgao   := Leitor.rCampo(tcInt, 'cOrgao');
        FretEvento.Items[i].FRetInfEvento.cStat    := Leitor.rCampo(tcInt, 'cStat');
        FretEvento.Items[i].FRetInfEvento.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');

        // Os campos abaixos seram retornados caso o cStat = 135 ou 136
        FretEvento.Items[i].FRetInfEvento.chMDFe      := Leitor.rCampo(tcStr, 'chMDFe');
        FretEvento.Items[i].FRetInfEvento.tpEvento    := StrToTpEventoMDFe(ok,Leitor.rCampo(tcStr, 'tpEvento'));
        FretEvento.Items[i].FRetInfEvento.xEvento     := Leitor.rCampo(tcStr, 'xEvento');
        FretEvento.Items[i].FRetInfEvento.nSeqEvento  := Leitor.rCampo(tcInt, 'nSeqEvento');
        FretEvento.Items[i].FRetInfEvento.dhRegEvento := Leitor.rCampo(tcDatHor, 'dhRegEvento');
        FretEvento.Items[i].FRetInfEvento.nProt       := Leitor.rCampo(tcStr, 'nProt');

        tpAmb   := FretEvento.Items[i].FRetInfEvento.tpAmb;
        cStat   := FretEvento.Items[i].FRetInfEvento.cStat;
        xMotivo := FretEvento.Items[i].FRetInfEvento.xMotivo;

        inc(i);
      end;

      if i = 0 then
        FretEvento.New;

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.
