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

unit pmdfeEnvEventoMDFe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador, pmdfeConsts,
  pmdfeEventoMDFe, pcnSignature;

type
  EventoException          = class(Exception);

  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    FRetInfEvento: TRetInfEvento;
    Fsignature: Tsignature;
  public
    constructor Create;
    destructor Destroy; override;
    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    function Add: TInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  { TEventoMDFe }

  TEventoMDFe = class(TObject)
  private
    FGerador: TGerador;
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: String;
    
    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: Boolean;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
    function LerFromIni(const AIniString: String): Boolean;

    property Gerador: TGerador             read FGerador write FGerador;
    property idLote: Int64                 read FidLote  write FidLote;
    property Evento: TInfEventoCollection  read FEvento  write SetEvento;
    property Versao: String                read FVersao  write FVersao;
  end;

implementation

uses
  IniFiles,
  ACBrDFeConsts,
  pmdfeRetEnvEventoMDFe, pmdfeConversaoMDFe, pmdfeMDFe,
  ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.DateTime,
  ACBrDFeUtil;

{ TEventoMDFe }

constructor TEventoMDFe.Create;
begin
  inherited Create;

  FGerador := TGerador.Create;
  FEvento := TInfEventoCollection.Create;
end;

destructor TEventoMDFe.Destroy;
begin
  FGerador.Free;
  FEvento.Free;

  inherited;
end;

function TEventoMDFe.GerarXML: Boolean;
var
  sDoc: String;
  i, j, Serie: Integer;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('eventoMDFe ' + NAME_SPACE_MDFE + ' versao="' + Versao + '"');

  Evento.Items[0].InfEvento.Id := 'ID'+ Evento.Items[0].InfEvento.TipoEvento +
                                   OnlyNumber(Evento.Items[0].InfEvento.chMDFe);

  if Evento.Items[0].InfEvento.nSeqEvento < 99 then
    Evento.Items[0].InfEvento.Id := Evento.Items[0].InfEvento.Id +
                         Format('%.2d', [Evento.Items[0].InfEvento.nSeqEvento])
  else
    Evento.Items[0].InfEvento.Id := Evento.Items[0].InfEvento.Id +
                         Format('%.3d', [Evento.Items[0].InfEvento.nSeqEvento]);

  Gerador.wGrupo('infEvento Id="' + Evento.Items[0].InfEvento.id + '"');

  if Length(Evento.Items[0].InfEvento.Id) < 54 then
    Gerador.wAlerta('EP04', 'ID', '', 'ID de Evento inválido');

  Gerador.wCampo(tcInt, 'EP05', 'cOrgao', 1, 2, 1, Evento.Items[0].InfEvento.cOrgao);
  Gerador.wCampo(tcStr, 'EP06', 'tpAmb ', 1, 1, 1, TpAmbToStr(Evento.Items[0].InfEvento.tpAmb), DSC_TPAMB);

  sDoc := OnlyNumber(Evento.Items[0].InfEvento.CNPJCPF);

  // Verifica a Série do Documento, caso esteja no intervalo de 910-969
  // o emitente é pessoa fisica, logo na chave temos um CPF.
  Serie := ExtrairSerieChaveAcesso(Evento.Items[0].InfEvento.chMDFe);

  if (Length(sDoc) = 14) and (Serie >= 910) and (Serie <= 969) then
    sDoc := Copy(sDoc, 4, 11);

  case Length(sDoc) of
    14: begin
          Gerador.wCampo(tcStr, 'EP07', 'CNPJ', 14, 14, 1, sDoc , DSC_CNPJ);
          if not ValidarCNPJ(sDoc) then
            Gerador.wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
        end;
    11: begin
          Gerador.wCampo(tcStr, 'EP07', 'CPF ', 11, 11, 1, sDoc, DSC_CPF);
          if not ValidarCPF(sDoc) then
            Gerador.wAlerta('HP11', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
        end;
  end;

  Gerador.wCampo(tcStr, 'EP08', 'chMDFe', 44, 44, 1, Evento.Items[0].InfEvento.chMDFe, DSC_CHAVE);

  if not ValidarChave(Evento.Items[0].InfEvento.chMDFe) then
    Gerador.wAlerta('EP08', 'chMDFe', '', 'Chave de MDFe inválida');

  if Versao = '3.00' then
    Gerador.wCampo(tcStr, 'EP09', 'dhEvento', 01, 25, 1,
     FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento.Items[0].InfEvento.dhEvento)
                        + GetUTC(CodigoUFparaUF(Evento.Items[0].InfEvento.cOrgao),
                                            Evento.Items[0].InfEvento.dhEvento))
  else
    Gerador.wCampo(tcStr, 'EP09', 'dhEvento', 01, 25, 1,
     FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento.Items[0].InfEvento.dhEvento));

  Gerador.wCampo(tcInt, 'EP10', 'tpEvento  ', 6, 6, 1, Evento.Items[0].InfEvento.TipoEvento);
  Gerador.wCampo(tcInt, 'EP11', 'nSeqEvento', 1, 3, 1, Evento.Items[0].InfEvento.nSeqEvento);

  Gerador.wGrupo('detEvento versaoEvento="' + Versao + '"');

  case Evento.Items[0].InfEvento.tpEvento of
   teCancelamento:
     begin
       Gerador.wGrupo('evCancMDFe');
       Gerador.wCampo(tcStr, 'EP02', 'descEvento', 005, 012, 1, Evento.Items[0].InfEvento.DescEvento);
       Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 015, 015, 1, Evento.Items[0].InfEvento.detEvento.nProt);
       Gerador.wCampo(tcStr, 'EP04', 'xJust     ', 015, 255, 1, Evento.Items[0].InfEvento.detEvento.xJust);
       Gerador.wGrupo('/evCancMDFe');
     end;

   teEncerramento:
     begin
       Gerador.wGrupo('evEncMDFe');
       Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 12, 1, Evento.Items[0].InfEvento.DescEvento);
       Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 15, 1, Evento.Items[0].InfEvento.detEvento.nProt);
       Gerador.wCampo(tcDat, 'EP04', 'dtEnc     ', 10, 10, 1, Evento.Items[0].InfEvento.detEvento.dtEnc);
       Gerador.wCampo(tcInt, 'EP05', 'cUF       ', 02, 02, 1, Evento.Items[0].InfEvento.detEvento.cUF);
       Gerador.wCampo(tcInt, 'EP06', 'cMun      ', 07, 07, 1, Evento.Items[0].InfEvento.detEvento.cMun);

       if Evento.Items[0].InfEvento.detEvento.indEncPorTerceiro = tiSim  then
         Gerador.wCampo(tcStr, 'EP07', 'indEncPorTerceiro', 1, 1, 1, '1');

       Gerador.wGrupo('/evEncMDFe');
     end;

   teInclusaoCondutor:
     begin
       Gerador.wGrupo('evIncCondutorMDFe');
       Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 17, 1, Evento.Items[0].InfEvento.DescEvento);
       Gerador.wGrupo('condutor');
       Gerador.wCampo(tcStr, 'EP04', 'xNome     ', 01, 60, 1, Evento.Items[0].InfEvento.detEvento.xNome);
       Gerador.wCampo(tcStr, 'EP05', 'CPF       ', 11, 11, 1, Evento.Items[0].InfEvento.detEvento.CPF);
       Gerador.wGrupo('/condutor');
       Gerador.wGrupo('/evIncCondutorMDFe');
     end;

   teInclusaoDFe:
     begin
       Gerador.wGrupo('evIncDFeMDFe');
       Gerador.wCampo(tcStr, 'HP02', 'descEvento ', 05, 13, 1, Evento.Items[0].InfEvento.DescEvento);
       Gerador.wCampo(tcStr, 'HP03', 'nProt      ', 15, 15, 1, Evento.Items[0].InfEvento.detEvento.nProt);
       Gerador.wCampo(tcInt, 'HP04', 'cMunCarrega', 07, 07, 1, Evento.Items[0].InfEvento.detEvento.cMunCarrega);
       Gerador.wCampo(tcStr, 'HP05', 'xMunCarrega', 02, 60, 1, Evento.Items[0].InfEvento.detEvento.xMunCarrega);

       for i := 0 to Evento.Items[0].FInfEvento.detEvento.infDoc.Count - 1 do
       begin
         Gerador.wGrupo('infDoc');
         Gerador.wCampo(tcInt, 'HP07', 'cMunDescarga', 07, 07, 1, Evento.Items[0].InfEvento.detEvento.infDoc.Items[i].cMunDescarga);
         Gerador.wCampo(tcStr, 'HP08', 'xMunDescarga', 02, 60, 1, Evento.Items[0].InfEvento.detEvento.infDoc.Items[i].xMunDescarga);
         Gerador.wCampo(tcStr, 'HP09', 'chNFe       ', 44, 44, 1, Evento.Items[0].InfEvento.detEvento.infDoc.Items[i].chNFe);
         Gerador.wGrupo('/infDoc');
       end;

       Gerador.wGrupo('/evIncDFeMDFe');
     end;

   tePagamentoOperacao:
     begin
       Gerador.wGrupo('evPagtoOperMDFe');
       Gerador.wCampo(tcStr, 'HP02', 'descEvento', 05, 24, 1, Evento.Items[0].InfEvento.DescEvento);
       Gerador.wCampo(tcStr, 'HP03', 'nProt     ', 15, 15, 1, Evento.Items[0].InfEvento.detEvento.nProt);

       Gerador.wGrupo('infViagens', '#');
       Gerador.wCampo(tcInt, '#', 'qtdViagens', 5, 5, 1, Evento.Items[0].InfEvento.detEvento.infViagens.qtdViagens);
       Gerador.wCampo(tcInt, '#', 'nroViagem ', 5, 5, 1, Evento.Items[0].InfEvento.detEvento.infViagens.nroViagem);
       Gerador.wGrupo('/infViagens');

       for i := 0 to Evento.Items[0].InfEvento.detEvento.infPag.Count - 1 do
       begin
         with Evento.Items[0].InfEvento.detEvento.infPag[i] do
         begin
           Gerador.wGrupo('infPag', '#');

           Gerador.wCampo(tcStr, '#', 'xNome', 02, 60, 0, xNome, DSC_XNOME);

           if idEstrangeiro <> '' then
             Gerador.wCampo(tcStr, '#', 'idEstrangeiro', 02, 20, 0, idEstrangeiro, DSC_IDESTRANGEIRO)
           else
             Gerador.wCampoCNPJCPF('#', '#', CNPJCPF);

           // Componentes de Pagamento do Frete
           for j := 0 to Comp.Count - 1 do
           begin
             Gerador.wGrupo('Comp', '#');
             Gerador.wCampo(tcStr, '#', 'tpComp', 02, 02, 1, TCompToStr(Comp[j].tpComp), DSC_TPCOMP);
             Gerador.wCampo(tcDe2, '#', 'vComp ', 01, 15, 1, Comp[j].vComp, DSC_VCOMP);
             Gerador.wCampo(tcStr, '#', 'xComp ', 02, 60, 0, Comp[j].xComp, DSC_XCOMP);
             Gerador.wGrupo('/Comp');
           end;

           Gerador.wCampo(tcDe2, '#', 'vContrato', 01, 15, 1, vContrato, DSC_VCONTRATO);
           Gerador.wCampo(tcStr, '#', 'indPag   ', 01, 01, 1, TIndPagToStr(indPag), DSC_INDPAG);
           Gerador.wCampo(tcDe2, '#', 'vAdiant  ', 01, 15, 0, vAdiant, DSC_VADIANT);

           if indAntecipaAdiant = tiSim then
             Gerador.wCampo(tcStr, '#', 'indAntecipaAdiant', 1, 1, 1, '1');

           // Informações do pagamento a prazo. Obs: Informar somente se indPag for à Prazo
           if indPag = ipPrazo then
           begin
             for j := 0 to infPrazo.Count - 1 do
             begin
               Gerador.wGrupo('infPrazo', '#');
               Gerador.wCampo(tcStr, '#', 'nParcela', 03, 03, 1, FormatFloat('000', infPrazo[j].nParcela), DSC_NPARCELA);
               Gerador.wCampo(tcDat, '#', 'dVenc   ', 10, 10, 1, infPrazo[j].dVenc, DSC_DVENC);
               Gerador.wCampo(tcDe2, '#', 'vParcela', 01, 15, 1, infPrazo[j].vParcela, DSC_VPARCELA);
               Gerador.wGrupo('/infPrazo');
             end;
           end;

           Gerador.wCampo(tcStr, '#', 'tpAntecip', 1, 1, 0, tpAntecipToStr(tpAntecip), DSC_TPANTECIP);

           Gerador.wGrupo('infBanc', '#');

           if infBanc.PIX <> '' then
             Gerador.wCampo(tcStr, '#', 'PIX', 2, 60, 1, infBanc.PIX, DSC_PIX)
           else
           begin
             if infBanc.CNPJIPEF <> '' then
               Gerador.wCampo(tcStr, '#', 'CNPJIPEF', 14, 14, 1, infBanc.CNPJIPEF, DSC_CNPJIPEF)
             else
             begin
               Gerador.wCampo(tcStr, '#', 'codBanco  ', 3, 05, 1, infBanc.codBanco, DSC_CODBANCO);
               Gerador.wCampo(tcStr, '#', 'codAgencia', 1, 10, 1, infBanc.codAgencia, DSC_CODAGENCIA);
             end;
           end;

           Gerador.wGrupo('/infBanc');

           Gerador.wGrupo('/infPag');
         end;
       end;

       Gerador.wGrupo('/evPagtoOperMDFe');
     end;

    teAlteracaoPagtoServMDFe:
      begin
        Gerador.wGrupo('evAlteracaoPagtoServMDFe');
        Gerador.wCampo(tcStr, 'HP02', 'descEvento', 05, 24, 1, Evento.Items[0].InfEvento.DescEvento);
        Gerador.wCampo(tcStr, 'HP03', 'nProt     ', 15, 15, 1, Evento.Items[0].InfEvento.detEvento.nProt);

        with Evento.Items[0].InfEvento.detEvento.infPag[0] do
        begin
          Gerador.wGrupo('infPag', '#');

          Gerador.wCampo(tcStr, '#', 'xNome', 02, 60, 0, xNome, DSC_XNOME);

          if idEstrangeiro <> '' then
            Gerador.wCampo(tcStr, '#', 'idEstrangeiro', 02, 20, 0, idEstrangeiro, DSC_IDESTRANGEIRO)
          else
            Gerador.wCampoCNPJCPF('#', '#', CNPJCPF);

          // Componentes de Pagamento do Frete
          for j := 0 to Comp.Count - 1 do
          begin
            Gerador.wGrupo('Comp', '#');
            Gerador.wCampo(tcStr, '#', 'tpComp', 02, 02, 1, TCompToStr(Comp[j].tpComp), DSC_TPCOMP);
            Gerador.wCampo(tcDe2, '#', 'vComp ', 01, 15, 1, Comp[j].vComp, DSC_VCOMP);
            Gerador.wCampo(tcStr, '#', 'xComp ', 02, 60, 0, Comp[j].xComp, DSC_XCOMP);
            Gerador.wGrupo('/Comp');
          end;

          Gerador.wCampo(tcDe2, '#', 'vContrato', 01, 15, 1, vContrato, DSC_VCONTRATO);
          Gerador.wCampo(tcStr, '#', 'indPag   ', 01, 01, 1, TIndPagToStr(indPag), DSC_INDPAG);
          Gerador.wCampo(tcDe2, '#', 'vAdiant  ', 01, 15, 0, vAdiant, DSC_VADIANT);

          if indAntecipaAdiant = tiSim then
            Gerador.wCampo(tcStr, '#', 'indAntecipaAdiant', 1, 1, 1, '1');

          // Informações do pagamento a prazo. Obs: Informar somente se indPag for à Prazo
          if indPag = ipPrazo then
          begin
            for j := 0 to infPrazo.Count - 1 do
            begin
              Gerador.wGrupo('infPrazo', '#');
              Gerador.wCampo(tcStr, '#', 'nParcela', 03, 03, 1, FormatFloat('000', infPrazo[j].nParcela), DSC_NPARCELA);
              Gerador.wCampo(tcDat, '#', 'dVenc   ', 10, 10, 1, infPrazo[j].dVenc, DSC_DVENC);
              Gerador.wCampo(tcDe2, '#', 'vParcela', 01, 15, 1, infPrazo[j].vParcela, DSC_VPARCELA);
              Gerador.wGrupo('/infPrazo');
            end;
          end;

          Gerador.wCampo(tcStr, '#', 'tpAntecip', 1, 1, 0, tpAntecipToStr(tpAntecip), DSC_TPANTECIP);

          Gerador.wGrupo('infBanc', '#');

          if infBanc.PIX <> '' then
            Gerador.wCampo(tcStr, '#', 'PIX', 2, 60, 1, infBanc.PIX, DSC_PIX)
          else
          begin
            if infBanc.CNPJIPEF <> '' then
              Gerador.wCampo(tcStr, '#', 'CNPJIPEF', 14, 14, 1, infBanc.CNPJIPEF, DSC_CNPJIPEF)
            else
            begin
              Gerador.wCampo(tcStr, '#', 'codBanco  ', 3, 05, 1, infBanc.codBanco, DSC_CODBANCO);
              Gerador.wCampo(tcStr, '#', 'codAgencia', 1, 10, 1, infBanc.codAgencia, DSC_CODAGENCIA);
            end;
          end;

          Gerador.wGrupo('/infBanc');

          Gerador.wGrupo('/infPag');
         end;

        Gerador.wGrupo('/evAlteracaoPagtoServMDFe');
      end;

    teConfirmaServMDFe:
      begin
        Gerador.wGrupo('evConfirmaServMDFe');
        Gerador.wCampo(tcStr, 'HP02', 'descEvento', 05, 24, 1, Evento.Items[0].InfEvento.DescEvento);
        Gerador.wCampo(tcStr, 'HP03', 'nProt     ', 15, 15, 1, Evento.Items[0].InfEvento.detEvento.nProt);
        Gerador.wGrupo('/evConfirmaServMDFe');
      end;
  end;

  Gerador.wGrupo('/detEvento');
  Gerador.wGrupo('/infEvento');

  if Evento.Items[0].signature.URI <> '' then
  begin
    Evento.Items[0].signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
    Evento.Items[0].signature.GerarXML;
    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Evento.Items[0].signature.Gerador.ArquivoFormatoXML;
  end;

  Gerador.wGrupo('/eventoMDFe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TEventoMDFe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoMDFe.LerXML(const CaminhoArquivo: String): Boolean;
var
  ArqEvento: TStringList;
begin
  ArqEvento := TStringList.Create;
  try
    ArqEvento.LoadFromFile(CaminhoArquivo);
    Result := LerXMLFromString(ArqEvento.Text);
  finally
    ArqEvento.Free;
  end;
end;

function TEventoMDFe.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoMDFe: TRetEventoMDFe;
  i, j: Integer;
begin
  RetEventoMDFe := TRetEventoMDFe.Create;

  try
    RetEventoMDFe.Leitor.Arquivo := AXML;
    Result := RetEventoMDFe.LerXml;

    with FEvento.New do
    begin
      infEvento.Id         := RetEventoMDFe.InfEvento.Id;
      InfEvento.cOrgao     := RetEventoMDFe.InfEvento.cOrgao;
      infEvento.tpAmb      := RetEventoMDFe.InfEvento.tpAmb;
      infEvento.CNPJCPF    := RetEventoMDFe.InfEvento.CNPJCPF;
      infEvento.chMDFe     := RetEventoMDFe.InfEvento.chMDFe;
      infEvento.dhEvento   := RetEventoMDFe.InfEvento.dhEvento;
      infEvento.tpEvento   := RetEventoMDFe.InfEvento.tpEvento;
      infEvento.nSeqEvento := RetEventoMDFe.InfEvento.nSeqEvento;

      infEvento.VersaoEvento         := RetEventoMDFe.InfEvento.VersaoEvento;
      infEvento.detEvento.descEvento := RetEventoMDFe.InfEvento.detEvento.descEvento;
      infEvento.detEvento.nProt      := RetEventoMDFe.InfEvento.detEvento.nProt;
      infEvento.detEvento.dtEnc      := RetEventoMDFe.InfEvento.detEvento.dtEnc;
      infEvento.detEvento.cUF        := RetEventoMDFe.InfEvento.detEvento.cUF;
      infEvento.detEvento.cMun       := RetEventoMDFe.InfEvento.detEvento.cMun;
      infEvento.detEvento.xJust      := RetEventoMDFe.InfEvento.detEvento.xJust;
      infEvento.detEvento.xNome      := RetEventoMDFe.InfEvento.detEvento.xNome;
      infEvento.detEvento.CPF        := RetEventoMDFe.InfEvento.detEvento.CPF;

      infEvento.detEvento.indEncPorTerceiro := RetEventoMDFe.InfEvento.detEvento.indEncPorTerceiro;

      infEvento.detEvento.cMunCarrega := RetEventoMDFe.InfEvento.detEvento.cMunCarrega;
      infEvento.detEvento.xMunCarrega := RetEventoMDFe.InfEvento.detEvento.xMunCarrega;

      InfEvento.detEvento.infViagens.qtdViagens := RetEventoMDFe.InfEvento.detEvento.infViagens.qtdViagens;
      InfEvento.detEvento.infViagens.nroViagem := RetEventoMDFe.InfEvento.detEvento.infViagens.nroViagem;

      signature.URI             := RetEventoMDFe.signature.URI;
      signature.DigestValue     := RetEventoMDFe.signature.DigestValue;
      signature.SignatureValue  := RetEventoMDFe.signature.SignatureValue;
      signature.X509Certificate := RetEventoMDFe.signature.X509Certificate;

      if RetEventoMDFe.retEvento.Count > 0 then
      begin
        FRetInfEvento.Id          := RetEventoMDFe.retEvento.Items[0].RetInfEvento.Id;
        FRetInfEvento.tpAmb       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.tpAmb;
        FRetInfEvento.verAplic    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.verAplic;
        FRetInfEvento.cOrgao      := RetEventoMDFe.retEvento.Items[0].RetInfEvento.cOrgao;
        FRetInfEvento.cStat       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.cStat;
        FRetInfEvento.xMotivo     := RetEventoMDFe.retEvento.Items[0].RetInfEvento.xMotivo;
        FRetInfEvento.chMDFe      := RetEventoMDFe.retEvento.Items[0].RetInfEvento.chMDFe;
        FRetInfEvento.tpEvento    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.tpEvento;
        FRetInfEvento.xEvento     := RetEventoMDFe.retEvento.Items[0].RetInfEvento.xEvento;
        FRetInfEvento.nSeqEvento  := RetEventoMDFe.retEvento.Items[0].RetInfEvento.nSeqEvento;
        FRetInfEvento.CNPJDest    := RetEventoMDFe.retEvento.Items[0].RetInfEvento.CNPJDest;
        FRetInfEvento.emailDest   := RetEventoMDFe.retEvento.Items[0].RetInfEvento.emailDest;
        FRetInfEvento.dhRegEvento := RetEventoMDFe.retEvento.Items[0].RetInfEvento.dhRegEvento;
        FRetInfEvento.nProt       := RetEventoMDFe.retEvento.Items[0].RetInfEvento.nProt;
        FRetInfEvento.XML         := RetEventoMDFe.retEvento.Items[0].RetInfEvento.XML;
      end;

      for i := 0 to RetEventoMDFe.InfEvento.detEvento.infDoc.Count -1 do
      begin
        infEvento.detEvento.infDoc.New;

        infEvento.detEvento.infDoc[i].cMunDescarga := RetEventoMDFe.InfEvento.detEvento.infDoc[i].cMunDescarga;
        infEvento.detEvento.infDoc[i].xMunDescarga := RetEventoMDFe.InfEvento.detEvento.infDoc[i].xMunDescarga;
        infEvento.detEvento.infDoc[i].chNFe        := RetEventoMDFe.InfEvento.detEvento.infDoc[i].chNFe;
      end;

      for i := 0 to RetEventoMDFe.InfEvento.detEvento.infPag.Count - 1 do
      begin
        infEvento.detEvento.infPag.New;

        InfEvento.detEvento.infPag[i].xNome         := RetEventoMDFe.InfEvento.detEvento.infPag[i].xNome;
        InfEvento.detEvento.infPag[i].idEstrangeiro := RetEventoMDFe.InfEvento.detEvento.infPag[i].idEstrangeiro;
        InfEvento.detEvento.infPag[i].CNPJCPF       := RetEventoMDFe.InfEvento.detEvento.infPag[i].CNPJCPF;

        for j := 0 to RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp.Count - 1 do
        begin
          InfEvento.detEvento.infPag[i].Comp.New;

          InfEvento.detEvento.infPag[i].Comp[j].tpComp := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].tpComp;
          InfEvento.detEvento.infPag[i].Comp[j].vComp  := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].vComp;
          InfEvento.detEvento.infPag[i].Comp[j].xComp  := RetEventoMDFe.InfEvento.detEvento.infPag[i].Comp[j].xComp;
        end;

        InfEvento.detEvento.infPag[i].vContrato := RetEventoMDFe.InfEvento.detEvento.infPag[i].vContrato;
        InfEvento.detEvento.infPag[i].indPag    := RetEventoMDFe.InfEvento.detEvento.infPag[i].indPag;
        InfEvento.detEvento.infPag[i].vAdiant   := RetEventoMDFe.InfEvento.detEvento.infPag[i].vAdiant;

        InfEvento.detEvento.infPag[i].indAntecipaAdiant := RetEventoMDFe.InfEvento.detEvento.infPag[i].indAntecipaAdiant;
        InfEvento.detEvento.infPag[i].tpAntecip := RetEventoMDFe.InfEvento.detEvento.infPag[i].tpAntecip;

        if InfEvento.detEvento.infPag[i].indPag = ipPrazo then
        begin
          for j := 0 to RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo.Count - 1 do
          begin
            InfEvento.detEvento.infPag[i].infPrazo.New;

            InfEvento.detEvento.infPag[i].infPrazo[j].nParcela := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].nParcela;
            InfEvento.detEvento.infPag[i].infPrazo[j].dVenc    := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].dVenc;
            InfEvento.detEvento.infPag[i].infPrazo[j].vParcela := RetEventoMDFe.InfEvento.detEvento.infPag[i].infPrazo[j].vParcela;
          end;
        end;

        InfEvento.detEvento.infPag[i].infBanc.PIX        := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.PIX;
        InfEvento.detEvento.infPag[i].infBanc.CNPJIPEF   := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.CNPJIPEF;
        InfEvento.detEvento.infPag[i].infBanc.codBanco   := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.codBanco;
        InfEvento.detEvento.infPag[i].infBanc.codAgencia := RetEventoMDFe.InfEvento.detEvento.infPag[i].infBanc.codAgencia;
      end;
    end;
  finally
    RetEventoMDFe.Free;
  end;
end;

function TEventoMDFe.ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
begin
  case tpEvento of
    teCancelamento:     Result := Evento.Items[0].InfEvento.chMDFe + '-can-eve.xml';
    teEncerramento:     Result := Evento.Items[0].InfEvento.chMDFe + '-ped-eve.xml';
    teInclusaoCondutor,
    teInclusaoDFe:      Result := Evento.Items[0].InfEvento.chMDFe + '-inc-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoMDFe.LerFromIni(const AIniString: String): Boolean;
var
  I, J, K: Integer;
  sSecao, sFim: String;
  INIRec: TMemIniFile;
  Ok: Boolean;
  ItemInfDoc: TInfDocCollectionItem;
  ItemInfPag: TinfPagCollectionItem;
  ItemComp: TCompCollectionItem;
  ItemInfPrazo: TInfPrazoCollectionItem;
begin
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO'+IntToStrZero(I,3);
      sFim   := INIRec.ReadString(sSecao, 'chMDFe', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;

      with Self.Evento.New do
      begin
        infEvento.chMDFe     := INIRec.ReadString(sSecao, 'chMDFe', '');
        infEvento.cOrgao     := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJCPF    := INIRec.ReadString(sSecao, 'CNPJCPF', '');
        infEvento.dhEvento   := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento   := StrToTpEventoMDFe(Ok, INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);

        // Usado no detalhamento do evento
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
        InfEvento.detEvento.dtEnc := StringToDateTime(INIRec.ReadString(sSecao, 'dtEnc', ''));
        InfEvento.detEvento.cUF   := INIRec.ReadInteger(sSecao, 'cUF', 0);
        InfEvento.detEvento.cMun  := INIRec.ReadInteger(sSecao, 'cMun', 0);
        infEvento.detEvento.xNome := INIRec.ReadString(sSecao, 'xNome', '');
        infEvento.detEvento.CPF   := INIRec.ReadString(sSecao, 'CPF', '');

        infEvento.detEvento.indEncPorTerceiro := StrToTIndicador(Ok, INIRec.ReadString(sSecao, 'indEncPorTerceiro', '0'));

        infEvento.detEvento.cMunCarrega := INIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
        infEvento.detEvento.xMunCarrega := INIRec.ReadString(sSecao, 'xMunCarrega', '');

        Self.Evento.Items[I-1].InfEvento.detEvento.infDoc.Clear;

        J := 1;
        while true do
        begin
          // J varia de 0000 até 2000
          sSecao := 'infDoc' + IntToStrZero(J, 4);
          sFim   := INIRec.ReadString(sSecao, 'chNFe', 'FIM');
          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          ItemInfDoc := Self.Evento.Items[I-1].InfEvento.detEvento.infDoc.New;

          ItemInfDoc.cMunDescarga := INIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
          ItemInfDoc.xMunDescarga := INIRec.ReadString(sSecao, 'xMunDescarga', '');
          ItemInfDoc.chNFe        := sFim;

          Inc(J);
        end;

        sSecao := 'infViagens';

        if INIRec.SectionExists(sSecao) then
        begin
          Self.Evento.Items[I-1].InfEvento.detEvento.infViagens.qtdViagens := INIRec.ReadInteger(sSecao, 'qtdViagens', 0);
          Self.Evento.Items[I-1].InfEvento.detEvento.infViagens.nroViagem  := INIRec.ReadInteger(sSecao, 'nroViagem', 0);
        end;

        sSecao := 'infPag001';

        if INIRec.SectionExists(sSecao) then
        begin
          Self.Evento.Items[I-1].InfEvento.detEvento.infPag.Clear;

          J := 1;
          while true do
          begin
            sSecao := 'infPag' + IntToStrZero(J, 3);
            sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

            if sFim = 'FIM' then
              break;

            ItemInfPag := Self.Evento.Items[I-1].InfEvento.detEvento.infPag.New;

            ItemInfPag.xNome         := INIRec.ReadString(sSecao, 'xNome', '');
            ItemInfPag.idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');

            if ItemInfPag.idEstrangeiro = '' then
              ItemInfPag.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');

            ItemInfPag.vContrato := StringToFloatDef(INIRec.ReadString(sSecao, 'vContrato', ''), 0 );
            ItemInfPag.indPag    := StrToTIndPag(ok, INIRec.ReadString(sSecao, 'indPag', '0'));
            ItemInfPag.vAdiant   := StringToFloatDef(INIRec.ReadString(sSecao, 'vAdiant', ''), 0 );

            ItemInfPag.indAntecipaAdiant := StrToTIndicador(ok, INIRec.ReadString(sSecao, 'indAntecipaAdiant', '0'));
            ItemInfPag.tpAntecip := StrTotpAntecip(ok, INIRec.ReadString(sSecao, 'tpAntecip', ''));

            K := 1;
            while true do
            begin
              sSecao := 'Comp' + IntToStrZero(J, 3) + IntToStrZero(K, 3);
              sFim   := INIRec.ReadString(sSecao, 'vComp', 'FIM');

              if sFim = 'FIM' then
                break;

              ItemComp := ItemInfPag.Comp.New;

              ItemComp.tpComp := StrToTComp(ok, INIRec.ReadString(sSecao, 'tpComp', '01'));
              ItemComp.vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0 );
              ItemComp.xComp  := INIRec.ReadString(sSecao, 'xComp', '');

              Inc(K);
            end;

            if ItemInfPag.indPag = ipPrazo then
            begin
              K := 1;
              while true do
              begin
                sSecao := 'infPrazo' + IntToStrZero(J, 3) + IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'vParcela', 'FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfPrazo := ItemInfPag.infPrazo.New;

                ItemInfPrazo.nParcela := INIRec.ReadInteger(sSecao, 'nParcela', 1);
                ItemInfPrazo.dVenc    := StringToDateTime(INIRec.ReadString(sSecao, 'dVenc', '0'));
                ItemInfPrazo.vParcela := StringToFloatDef(INIRec.ReadString(sSecao, 'vParcela', ''), 0 );

                Inc(K);
              end;
            end;

            sSecao := 'infBanc' + IntToStrZero(J, 3);

            if INIRec.SectionExists(sSecao) then
            begin
              ItemInfPag.infBanc.PIX := INIRec.ReadString(sSecao, 'PIX', '');

              if ItemInfPag.infBanc.PIX = '' then
              begin
                ItemInfPag.infBanc.CNPJIPEF := INIRec.ReadString(sSecao, 'CNPJIPEF', '');

                if ItemInfPag.infBanc.CNPJIPEF = '' then
                begin
                  ItemInfPag.infBanc.codBanco   := INIRec.ReadString(sSecao, 'codBanco', '');
                  ItemInfPag.infBanc.codAgencia := INIRec.ReadString(sSecao, 'codAgencia', '');
                end;
              end;
            end;

            Inc(J);
          end;
        end;
      end;

      Inc(I);
    end;
  finally
    INIRec.Free;
  end;

  Result := True;
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfEventoCollection.New: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  inherited Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  Fsignature.Free;
  FRetInfEvento.Free;
  inherited;
end;

end.
