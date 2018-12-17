////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnEnvEventoBPe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnConsts,
  pcnEventoBPe, pcnBPeConsts, pcnSignature;

type
  TInfEventoCollection     = class;
  TInfEventoCollectionItem = class;
  TEventoBPe               = class;
  EventoException          = class(Exception);

  TInfEventoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TInfEventoCollectionItem = class(TCollectionItem)
  private
    FInfEvento: TInfEvento;
    FRetInfEvento: TRetInfEvento;
    Fsignature: Tsignature;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  { TEventoBPe }

  TEventoBPe = class(TPersistent)
  private
    FGerador: TGerador;
    FidLote: Integer;
    FEvento: TInfEventoCollection;
    FVersao: String;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function LerFromIni(const AIniString: String): Boolean;
  published
    property Gerador: TGerador            read FGerador write FGerador;
    property idLote: Integer              read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: String               read FVersao  write FVersao;
  end;

implementation

uses
  IniFiles,
  pcnAuxiliar, pcnRetEnvEventoBPe, pcnConversaoBPe,
  ACBrUtil, ACBrDFeUtil;

{ TEventoBPe }

constructor TEventoBPe.Create;
begin
  FGerador := TGerador.Create;
  FEvento  := TInfEventoCollection.Create(Self);
end;

destructor TEventoBPe.Destroy;
begin
  FGerador.Free;
  FEvento.Free;
  inherited;
end;

function TEventoBPe.GerarXML: Boolean;
var
  i: Integer;
  sDoc: String;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('eventoBPe ' + NAME_SPACE_BPE + ' versao="' + Versao + '"');

  for i := 0 to Evento.Count - 1 do
  begin
    Evento.Items[i].InfEvento.id := 'ID' +
                                      Evento.Items[i].InfEvento.TipoEvento +
                                      OnlyNumber(Evento.Items[i].InfEvento.chBPe) +
                                      Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

    Gerador.wGrupo('infEvento Id="' + Evento.Items[i].InfEvento.id + '"');

    if Length(Evento.Items[i].InfEvento.id) < 54 then
      Gerador.wAlerta('EP04', 'ID', '', 'ID de Evento inválido');

    Gerador.wCampo(tcInt, 'EP05', 'cOrgao', 01, 02, 1, FEvento.Items[i].FInfEvento.cOrgao);
    Gerador.wCampo(tcStr, 'EP06', 'tpAmb ', 01, 01, 1, TpAmbToStr(Evento.Items[i].InfEvento.tpAmb), DSC_TPAMB);

    sDoc := OnlyNumber( Evento.Items[i].InfEvento.CNPJ );
    case Length( sDoc ) of
     14: begin
           Gerador.wCampo(tcStr, 'EP07', 'CNPJ', 14, 14, 1, sDoc , DSC_CNPJ);
           if not ValidarCNPJ( sDoc ) then Gerador.wAlerta('EP07', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
         end;
     11: begin
           Gerador.wCampo(tcStr, 'EP07', 'CPF', 11, 11, 1, sDoc, DSC_CPF);
           if not ValidarCPF( sDoc ) then Gerador.wAlerta('EP07', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
         end;
    end;

    Gerador.wCampo(tcStr, 'EP08', 'chBPe', 44, 44, 1, Evento.Items[i].InfEvento.chBPe, DSC_CHAVE);

    if not ValidarChave(Evento.Items[i].InfEvento.chBPe) then
      Gerador.wAlerta('EP08', 'chBPe', '', 'Chave de BPe inválida');

    Gerador.wCampo(tcStr, 'EP09', 'dhEvento  ', 01, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento.Items[i].InfEvento.dhEvento) +
                                                           GetUTC(CodigoParaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.dhEvento));
    Gerador.wCampo(tcInt, 'EP10', 'tpEvento  ', 06, 06, 1, Evento.Items[i].InfEvento.TipoEvento);
    Gerador.wCampo(tcInt, 'EP11', 'nSeqEvento', 01, 02, 1, Evento.Items[i].InfEvento.nSeqEvento);

    Gerador.wGrupo('detEvento versaoEvento="' +  Versao + '"');

    case Evento.Items[i].InfEvento.tpEvento of
      teCancelamento:
          begin
            Gerador.wGrupo('evCancBPe', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 060, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 015, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcStr, 'EP04', 'xJust     ', 15, 255, 1, Evento.Items[i].InfEvento.detEvento.xJust);
            Gerador.wGrupo('/evCancBPe');
          end;

      teNaoEmbarque:
          begin
            Gerador.wGrupo('evNaoEmbBPe', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 060, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 015, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcStr, 'EP04', 'xJust     ', 15, 255, 1, Evento.Items[i].InfEvento.detEvento.xJust);
            Gerador.wGrupo('/evNaoEmbBPe');
          end;
    end;
    Gerador.wGrupo('/detEvento');
    Gerador.wGrupo('/infEvento');

    if Evento.Items[i].signature.URI <> '' then
    begin
      Evento.Items[i].signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      Evento.Items[i].signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Evento.Items[i].signature.Gerador.ArquivoFormatoXML;
    end;

  end;
  Gerador.wGrupo('/eventoBPe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TEventoBPe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoBPe.LerXML(const CaminhoArquivo: String): Boolean;
var
  ArqEvento    : TStringList;
begin
  ArqEvento := TStringList.Create;
  try
     ArqEvento.LoadFromFile(CaminhoArquivo);
     Result := LerXMLFromString(ArqEvento.Text);
  finally
     ArqEvento.Free;
  end;
end;

function TEventoBPe.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoBPe : TRetEventoBPe;
begin
  RetEventoBPe := TRetEventoBPe.Create;
  try
     RetEventoBPe.Leitor.Arquivo := AXML;
     Result := RetEventoBPe.LerXml;
     with FEvento.Add do
      begin
        infEvento.ID            := RetEventoBPe.InfEvento.id;
        infEvento.cOrgao        := RetEventoBPe.InfEvento.cOrgao;
        infEvento.tpAmb         := RetEventoBPe.InfEvento.tpAmb;
        infEvento.CNPJ          := RetEventoBPe.InfEvento.CNPJ;
        infEvento.chBPe         := RetEventoBPe.InfEvento.chBPe;
        infEvento.dhEvento      := RetEventoBPe.InfEvento.dhEvento;
        infEvento.tpEvento      := RetEventoBPe.InfEvento.tpEvento;
        infEvento.nSeqEvento    := RetEventoBPe.InfEvento.nSeqEvento;
        infEvento.VersaoEvento  := RetEventoBPe.InfEvento.VersaoEvento;

        infEvento.DetEvento.xCorrecao := RetEventoBPe.InfEvento.DetEvento.xCorrecao;
        infEvento.DetEvento.xCondUso  := RetEventoBPe.InfEvento.DetEvento.xCondUso;
        infEvento.DetEvento.nProt     := RetEventoBPe.InfEvento.DetEvento.nProt;
        infEvento.DetEvento.xJust     := RetEventoBPe.InfEvento.DetEvento.xJust;

        infEvento.detEvento.cOrgaoAutor := RetEventoBPe.InfEvento.detEvento.cOrgaoAutor;
        infEvento.detEvento.tpAutor     := RetEventoBPe.InfEvento.detEvento.tpAutor;
        infEvento.detEvento.verAplic    := RetEventoBPe.InfEvento.detEvento.verAplic;
        infEvento.detEvento.dhEmi       := RetEventoBPe.InfEvento.detEvento.dhEmi;
        infEvento.detEvento.tpBPe       := RetEventoBPe.InfEvento.detEvento.tpBPe;
        infEvento.detEvento.IE          := RetEventoBPe.InfEvento.detEvento.IE;

        infEvento.detEvento.dest.UF            := RetEventoBPe.InfEvento.detEvento.dest.UF;
        infEvento.detEvento.dest.CNPJCPF       := RetEventoBPe.InfEvento.detEvento.dest.CNPJCPF;
        infEvento.detEvento.dest.idEstrangeiro := RetEventoBPe.InfEvento.detEvento.dest.idEstrangeiro;
        infEvento.detEvento.dest.IE            := RetEventoBPe.InfEvento.detEvento.dest.IE;

        infEvento.detEvento.vNF   := RetEventoBPe.InfEvento.detEvento.vNF;
        infEvento.detEvento.vICMS := RetEventoBPe.InfEvento.detEvento.vICMS;
        infEvento.detEvento.vST   := RetEventoBPe.InfEvento.detEvento.vST;

        signature.URI             := RetEventoBPe.signature.URI;
        signature.DigestValue     := RetEventoBPe.signature.DigestValue;
        signature.SignatureValue  := RetEventoBPe.signature.SignatureValue;
        signature.X509Certificate := RetEventoBPe.signature.X509Certificate;

        if RetEventoBPe.retEvento.Count > 0 then
         begin
           FRetInfEvento.Id := RetEventoBPe.retEvento.Items[0].RetInfEvento.Id;
           FRetInfEvento.tpAmb := RetEventoBPe.retEvento.Items[0].RetInfEvento.tpAmb;
           FRetInfEvento.verAplic := RetEventoBPe.retEvento.Items[0].RetInfEvento.verAplic;
           FRetInfEvento.cOrgao := RetEventoBPe.retEvento.Items[0].RetInfEvento.cOrgao;
           FRetInfEvento.cStat := RetEventoBPe.retEvento.Items[0].RetInfEvento.cStat;
           FRetInfEvento.xMotivo := RetEventoBPe.retEvento.Items[0].RetInfEvento.xMotivo;
           FRetInfEvento.chBPe := RetEventoBPe.retEvento.Items[0].RetInfEvento.chBPe;
           FRetInfEvento.tpEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.tpEvento;
           FRetInfEvento.xEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.xEvento;
           FRetInfEvento.nSeqEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.nSeqEvento;
           FRetInfEvento.CNPJDest := RetEventoBPe.retEvento.Items[0].RetInfEvento.CNPJDest;
           FRetInfEvento.emailDest := RetEventoBPe.retEvento.Items[0].RetInfEvento.emailDest;
           FRetInfEvento.dhRegEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.dhRegEvento;
           FRetInfEvento.nProt := RetEventoBPe.retEvento.Items[0].RetInfEvento.nProt;
           FRetInfEvento.XML := RetEventoBPe.retEvento.Items[0].RetInfEvento.XML;
         end;
      end;
  finally
     RetEventoBPe.Free;
  end;
end;

function TEventoBPe.LerFromIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim: String;
  ok: Boolean;
  I: Integer;
begin
  Result := False;
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO'+IntToStrZero(I,3);
      sFim   := INIRec.ReadString(sSecao, 'chCTe', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;

      with Self.Evento.Add do
      begin
        infEvento.chBPe              := INIRec.ReadString(sSecao, 'chBPe', '');
        infEvento.cOrgao             := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ               := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.dhEvento           := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento           := StrToTpEvento(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento         := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.detEvento.xCondUso := '';
        infEvento.detEvento.xJust    := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt    := INIRec.ReadString(sSecao, 'nProt', '');
      end;
      Inc(I);
    end;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfEventoCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TInfEventoCollectionItem);
end;

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited GetItem(Index));
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
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
