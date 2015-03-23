////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
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

{$I ACBr.inc}

unit pcnEnvEventoNFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnEventoNFe;

type
  TInfEventoCollection     = class;
  TInfEventoCollectionItem = class;
  TEventoNFe               = class;
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
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TEventoNFe = class(TPersistent)
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
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
  published
    property Gerador: TGerador            read FGerador write FGerador;
    property idLote: Integer              read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: String               read FVersao  write FVersao;
  end;

implementation

uses
  pcnRetEnvEventoNFe, pcnAuxiliar, pcnConversaoNFe,
  ACBrUtil;

{ TEventoNFe }

constructor TEventoNFe.Create;
begin
  FGerador := TGerador.Create;
  FEvento  := TInfEventoCollection.Create(Self);
end;

destructor TEventoNFe.Destroy;
begin
  FGerador.Free;
  FEvento.Free;
  inherited;
end;

function TEventoNFe.ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
begin
 case tpEvento of
    teCCe                       : Result := IntToStr(Self.idLote) + '-cce.xml';     // Carta de Correção Eletrônica
    teCancelamento              : Result := IntToStr(Self.idLote) + '-can-eve.xml'; // Cancelamento da NFe como Evento
    teManifDestCiencia,
    teManifDestConfirmacao,
    teManifDestDesconhecimento,
    teManifDestOperNaoRealizada : Result := IntToStr(Self.idLote) + '-man-des.xml'; // Manifestação do Destinatário
    teEPECNFe                   : Result := Evento.Items[0].InfEvento.chNFe + '-ped-epec.xml'; // EPEC
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
 end;
end;

function TEventoNFe.GerarXML: Boolean;
var
  i: Integer;
  sDoc, sModelo: String;
begin
  Result := False;

  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('envEvento ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcInt, 'HP03', 'idLote', 001, 015, 1, FidLote, DSC_IDLOTE);
  for i := 0 to Evento.Count - 1 do
  begin
    sModelo := Copy(OnlyNumber(Evento.Items[i].InfEvento.chNFe), 21, 2);

    Evento.Items[i].InfEvento.id := 'ID'+
                                      Evento.Items[i].InfEvento.TipoEvento +
                                      OnlyNumber(Evento.Items[i].InfEvento.chNFe) +
                                      Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

    Gerador.wGrupo('evento ' + NAME_SPACE + ' versao="' + Versao + '"');
    Gerador.wGrupo('infEvento Id="' + Evento.Items[i].InfEvento.id + '"');

    if Length(Evento.Items[i].InfEvento.id) < 54 then
      Gerador.wAlerta('HP07', 'ID', '', 'ID de Evento inválido');

    Gerador.wCampo(tcInt, 'HP08', 'cOrgao', 001, 002, 1, FEvento.Items[i].FInfEvento.cOrgao);
    Gerador.wCampo(tcStr, 'HP09', 'tpAmb', 001, 001,  1, TpAmbToStr(Evento.Items[i].InfEvento.tpAmb), DSC_TPAMB);

    // OnlyNumber ..estava executando 5 vezes na versao anterior
    // no techo de verificar se era cnpj ou cpf.
    sDoc := OnlyNumber( Evento.Items[i].InfEvento.CNPJ );
    case Length( sDoc ) of
     14: begin
           Gerador.wCampo(tcStr, 'HP10', 'CNPJ', 014, 014, 1, sDoc , DSC_CNPJ);
           if not ValidarCNPJ( sDoc ) then Gerador.wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
         end;
     11: begin
           Gerador.wCampo(tcStr, 'HP11', 'CPF', 011, 011, 1, sDoc, DSC_CPF);
           if not ValidarCPF( sDoc ) then Gerador.wAlerta('HP11', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
         end;
    end;
    Gerador.wCampo(tcStr,    'HP12', 'chNFe', 044, 044,      1, Evento.Items[i].InfEvento.chNFe, DSC_CHAVE);

    if not ValidarChave('NFe' + OnlyNumber(Evento.Items[i].InfEvento.chNFe)) then
      Gerador.wAlerta('HP12', 'chNFe', '', 'Chave de NFe inválida');

    Gerador.wCampo(tcStr,    'HP13', 'dhEvento', 001, 050,   1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Evento.Items[i].InfEvento.dhEvento)+
                                                                GetUTC(CodigoParaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.dhEvento));
    Gerador.wCampo(tcInt,    'HP14', 'tpEvento', 006, 006,   1, Evento.Items[i].InfEvento.TipoEvento);
    Gerador.wCampo(tcInt,    'HP15', 'nSeqEvento', 001, 002, 1, Evento.Items[i].InfEvento.nSeqEvento);
    Gerador.wCampo(tcStr,    'HP16', 'verEvento', 001, 004,  1, Evento.Items[i].InfEvento.versaoEvento);
    Gerador.wGrupo('detEvento versao="' +  Versao + '"');
    Gerador.wCampo(tcStr,    'HP19', 'descEvento', 005, 060, 1,  Evento.Items[i].InfEvento.DescEvento);
    case Evento.Items[i].InfEvento.tpEvento of
        teCCe:
          begin
            Gerador.wCampo(tcStr, 'HP20', 'xCorrecao', 015, 1000, 1,  Evento.Items[i].InfEvento.detEvento.xCorrecao);
            Gerador.wCampo(tcStr, 'HP20a', 'xCondUso', 001, 5000, 1,  Evento.Items[i].InfEvento.detEvento.xCondUso);
          end;
        teCancelamento:
          begin
            Gerador.wCampo(tcStr, 'HP20', 'nProt', 015, 015, 1,  Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcStr, 'HP21', 'xJust', 015, 255, 1,  Evento.Items[i].InfEvento.detEvento.xJust);
          end;
        teManifDestOperNaoRealizada:
          begin
            Gerador.wCampo(tcStr, 'HP21', 'xJust', 015, 255, 1,  Evento.Items[i].InfEvento.detEvento.xJust);
          end;
        teEPECNFe:
          begin
            Gerador.wCampo(tcInt, 'P20', 'cOrgaoAutor', 01, 02, 1, FEvento.Items[i].FInfEvento.detEvento.cOrgaoAutor);
            Gerador.wCampo(tcStr, 'P21', 'tpAutor',     01, 01, 1, TipoAutorToStr(Evento.Items[i].InfEvento.detEvento.tpAutor));
            Gerador.wCampo(tcStr, 'P22', 'verAplic',    01, 20, 1, Evento.Items[i].InfEvento.detEvento.verAplic);
            Gerador.wCampo(tcStr, 'P23', 'dhEmi',       01, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Evento.Items[i].InfEvento.detEvento.dhEmi)+
                                                                   GetUTC(CodigoParaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.detEvento.dhEmi));
            Gerador.wCampo(tcStr, 'P24', 'tpNF',        01, 01, 1, tpNFToStr(Evento.Items[i].InfEvento.detEvento.tpNF));
            Gerador.wCampo(tcStr, 'P25', 'IE',          02, 14, 1, Evento.Items[i].InfEvento.detEvento.IE);

            Gerador.wGrupo('dest');
            Gerador.wCampo(tcStr, 'P27', 'UF', 02, 02, 1, Evento.Items[i].InfEvento.detEvento.dest.UF);

            if (Evento.Items[i].InfEvento.detEvento.dest.idEstrangeiro = '') and
               (Evento.Items[i].InfEvento.detEvento.dest.UF <> 'EX') then
             begin
               sDoc := OnlyNumber( Evento.Items[i].InfEvento.detEvento.dest.CNPJCPF );
               case Length( sDoc ) of
                14 : begin
                       Gerador.wCampo(tcStr, 'P28', 'CNPJ', 014, 014, 1, sDoc , DSC_CNPJ);
                       if not ValidarCNPJ( sDoc ) then Gerador.wAlerta('P28', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
                     end;
                11 : begin
                       Gerador.wCampo(tcStr, 'P29', 'CPF', 011, 011, 1, sDoc, DSC_CPF);
                       if not ValidarCPF( sDoc ) then Gerador.wAlerta('P29', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
                     end;
               end;
             end
            else
             begin
                Gerador.wCampo(tcStr, 'P30', 'idEstrangeiro', 05, 20, 1, Evento.Items[i].InfEvento.detEvento.dest.idEstrangeiro);
             end;

            if (Evento.Items[i].InfEvento.detEvento.dest.IE <> '') and (sModelo = '55') then
              Gerador.wCampo(tcStr, 'P31', 'IE', 02, 14, 1, Evento.Items[i].InfEvento.detEvento.dest.IE);

//            Gerador.wGrupo('/dest');

            Gerador.wCampo(tcDe2, 'P32', 'vNF',   01, 15, 1, Evento.Items[i].InfEvento.detEvento.vNF, DSC_VNF);
            Gerador.wCampo(tcDe2, 'P33', 'vICMS', 01, 15, 1, Evento.Items[i].InfEvento.detEvento.vICMS, DSC_VICMS);

            if sModelo = '55' then
              Gerador.wCampo(tcDe2, 'P34', 'vST',   01, 15, 1, Evento.Items[i].InfEvento.detEvento.vST, DSC_VST);

            // Alterado em 22/07/2014 por Italo
            // para ficar em conformidade com o Schema
            Gerador.wGrupo('/dest');

          end;
    end;
    Gerador.wGrupo('/detEvento');
    Gerador.wGrupo('/infEvento');
    Gerador.wGrupo('/evento');
  end;
  Gerador.wGrupo('/envEvento');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TEventoNFe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoNFe.LerXML(const CaminhoArquivo: String): Boolean;
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

function TEventoNFe.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoNFe : TRetEventoNFe;
begin
  RetEventoNFe := TRetEventoNFe.Create;
  try
     RetEventoNFe.Leitor.Arquivo := AXML;
     Result := RetEventoNFe.LerXml;
     with FEvento.Add do
      begin
        infEvento.ID            := RetEventoNFe.InfEvento.id;
        infEvento.cOrgao        := RetEventoNFe.InfEvento.cOrgao;
        infEvento.tpAmb         := RetEventoNFe.InfEvento.tpAmb;
        infEvento.CNPJ          := RetEventoNFe.InfEvento.CNPJ;
        infEvento.chNFe         := RetEventoNFe.InfEvento.chNFe;
        infEvento.dhEvento      := RetEventoNFe.InfEvento.dhEvento;
        infEvento.tpEvento      := RetEventoNFe.InfEvento.tpEvento;
        infEvento.nSeqEvento    := RetEventoNFe.InfEvento.nSeqEvento;
        infEvento.VersaoEvento  := RetEventoNFe.InfEvento.VersaoEvento;

        infEvento.DetEvento.xCorrecao := RetEventoNFe.InfEvento.DetEvento.xCorrecao;
        infEvento.DetEvento.xCondUso  := RetEventoNFe.InfEvento.DetEvento.xCondUso;
        infEvento.DetEvento.nProt     := RetEventoNFe.InfEvento.DetEvento.nProt;
        infEvento.DetEvento.xJust     := RetEventoNFe.InfEvento.DetEvento.xJust;

        infEvento.detEvento.cOrgaoAutor := RetEventoNFe.InfEvento.detEvento.cOrgaoAutor;
        infEvento.detEvento.tpAutor     := RetEventoNFe.InfEvento.detEvento.tpAutor;
        infEvento.detEvento.verAplic    := RetEventoNFe.InfEvento.detEvento.verAplic;
        infEvento.detEvento.dhEmi       := RetEventoNFe.InfEvento.detEvento.dhEmi;
        infEvento.detEvento.tpNF        := RetEventoNFe.InfEvento.detEvento.tpNF;
        infEvento.detEvento.IE          := RetEventoNFe.InfEvento.detEvento.IE;

        infEvento.detEvento.dest.UF            := RetEventoNFe.InfEvento.detEvento.dest.UF;
        infEvento.detEvento.dest.CNPJCPF       := RetEventoNFe.InfEvento.detEvento.dest.CNPJCPF;
        infEvento.detEvento.dest.idEstrangeiro := RetEventoNFe.InfEvento.detEvento.dest.idEstrangeiro;
        infEvento.detEvento.dest.IE            := RetEventoNFe.InfEvento.detEvento.dest.IE;

        infEvento.detEvento.vNF   := RetEventoNFe.InfEvento.detEvento.vNF;
        infEvento.detEvento.vICMS := RetEventoNFe.InfEvento.detEvento.vICMS;
        infEvento.detEvento.vST   := RetEventoNFe.InfEvento.detEvento.vST;

        if RetEventoNFe.retEvento.Count > 0 then
         begin
           FRetInfEvento.Id := RetEventoNFe.retEvento.Items[0].RetInfEvento.Id;
           FRetInfEvento.tpAmb := RetEventoNFe.retEvento.Items[0].RetInfEvento.tpAmb;
           FRetInfEvento.verAplic := RetEventoNFe.retEvento.Items[0].RetInfEvento.verAplic;
           FRetInfEvento.cOrgao := RetEventoNFe.retEvento.Items[0].RetInfEvento.cOrgao;
           FRetInfEvento.cStat := RetEventoNFe.retEvento.Items[0].RetInfEvento.cStat;
           FRetInfEvento.xMotivo := RetEventoNFe.retEvento.Items[0].RetInfEvento.xMotivo;
           FRetInfEvento.chNFe := RetEventoNFe.retEvento.Items[0].RetInfEvento.chNFe;
           FRetInfEvento.tpEvento := RetEventoNFe.retEvento.Items[0].RetInfEvento.tpEvento;
           FRetInfEvento.xEvento := RetEventoNFe.retEvento.Items[0].RetInfEvento.xEvento;
           FRetInfEvento.nSeqEvento := RetEventoNFe.retEvento.Items[0].RetInfEvento.nSeqEvento;
           FRetInfEvento.CNPJDest := RetEventoNFe.retEvento.Items[0].RetInfEvento.CNPJDest;
           FRetInfEvento.emailDest := RetEventoNFe.retEvento.Items[0].RetInfEvento.emailDest;
           FRetInfEvento.dhRegEvento := RetEventoNFe.retEvento.Items[0].RetInfEvento.dhRegEvento;
           FRetInfEvento.nProt := RetEventoNFe.retEvento.Items[0].RetInfEvento.nProt;
         end;
      end;
  finally
     RetEventoNFe.Free;
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
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  FRetInfEvento.Free;
  inherited;
end;

end.
