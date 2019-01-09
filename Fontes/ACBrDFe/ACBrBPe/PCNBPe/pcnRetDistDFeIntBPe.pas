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

unit pcnRetDistDFeIntBPe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnConversaoBPe, pcnLeitor, synacode;

type
  TresBPe               = class;
  TresEvento            = class;
  TprocEvento           = class;
  TdocZipCollection     = class;
  TdocZipCollectionItem = class;
  TRetDistDFeInt        = class;

  TDetEventoCTe = class
  private
    FchCTe: String;
    Fmodal: TpcteModal;
    FdhEmi: TDateTime;
    FnProt: String;
    FdhRecbto: TDateTime;
  public
    property chCTe: String       read FchCTe    write FchCTe;
    property modal: TpcteModal   read Fmodal    write Fmodal;
    property dhEmi: TDateTime    read FdhEmi    write FdhEmi;
    property nProt: String       read FnProt    write FnProt;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
  end;

  TDetEventoEmit = class
  private
    FCNPJ: String;
    FIE: String;
    FxNome: String;
  public
    property CNPJ: String  read FCNPJ  write FCNPJ;
    property IE: String    read FIE    write FIE;
    property xNome: String read FxNome write FxNome;
  end;

  TprocEvento_DetEvento = class
  private
    FVersao: String;
    FDescEvento: String;
    FnProt: String;
    FxJust: String;
    FxCorrecao: String;

    FCTe: TDetEventoCTe;
    Femit: TDetEventoEmit;
  public
    constructor Create(AOwner: TprocEvento);
    destructor Destroy; override;

    property versao: String     read FVersao     write FVersao;
    property descEvento: String read FDescEvento write FDescEvento;
    property nProt: String      read FnProt      write FnProt;
    property xJust: String      read FxJust      write FxJust;
    property xCorrecao: String  read FxCorrecao  write FxCorrecao;

    property CTe: TDetEventoCTe   read FCTe  write FCTe;
    property emit: TDetEventoEmit read Femit write Femit;
  end;

  TprocEvento_RetInfEvento = class
  private
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchBPe: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FcOrgaoAutor: Integer;
    FdhRegEvento: TDateTime;
    FnProt: String;
  public
    property Id: String              read FId          write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: String        read FverAplic    write FverAplic;
    property cOrgao: Integer         read FcOrgao      write FcOrgao;
    property cStat: Integer          read FcStat       write FcStat;
    property xMotivo: String         read FxMotivo     write FxMotivo;
    property chBPe: String           read FchBPe       write FchBPe;
    property tpEvento: TpcnTpEvento  read FtpEvento    write FtpEvento;
    property xEvento: String         read FxEvento     write FxEvento;
    property nSeqEvento: Integer     read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String        read FCNPJDest    write FCNPJDest;
    property emailDest: String       read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer    read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime  read FdhRegEvento write FdhRegEvento;
    property nProt: String           read FnProt       write FnProt;
  end;

  TresBPe = class
  private
    FchBPe: String;
    FCNPJCPF: String;
    FxNome: String;
    FIE: String;
    FdhEmi: TDateTime;
    FtpBPe: TTipoBPe;
    FvNF: Currency;
    FdigVal: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FcSitBPe: TSituacaoDFe;
  public
    property chBPe: String            read FchBPe    write FchBPe;
    property CNPJCPF: String          read FCNPJCPF  write FCNPJCPF;
    property xNome: String            read FxNome    write FxNome;
    property IE: String               read FIE       write FIE;
    property dhEmi: TDateTime         read FdhEmi    write FdhEmi;
    property tpBPe: TTipoBPe          read FtpBPe    write FtpBPe;
    property vNF: Currency            read FvNF      write FvNF;
    property digVal: String           read FdigVal   write FdigVal;
    property dhRecbto: TDateTime      read FdhRecbto write FdhRecbto;
    property nProt: String            read FnProt    write FnProt;
    property cSitBPe: TSituacaoDFe    read FcSitBPe  write FcSitBPe;
  end;

  TresEvento = class
  private
    FcOrgao: Integer;
    FCNPJCPF: String;
    FchBPe: String;
    FdhEvento: TDateTime;
    FtpEvento: TpcnTpEvento;
    FnSeqEvento: ShortInt;
    FxEvento: String;
    FdhRecbto: TDateTime;
    FnProt: String;
  public
    property cOrgao: Integer        read FcOrgao     write FcOrgao;
    property CNPJCPF: String        read FCNPJCPF    write FCNPJCPF;
    property chBPe: String          read FchBPe      write FchBPe;
    property dhEvento: TDateTime    read FdhEvento   write FdhEvento;
    property tpEvento: TpcnTpEvento read FtpEvento   write FtpEvento;
    property nSeqEvento: ShortInt   read FnSeqEvento write FnSeqEvento;
    property xEvento: String        read FxEvento    write FxEvento;
    property dhRecbto: TDateTime    read FdhRecbto   write FdhRecbto;
    property nProt: String          read FnProt      write FnProt;
  end;

  TprocEvento = class
  private
    FId: String;
    FcOrgao: Integer;
    FtpAmb: TpcnTipoAmbiente;
    FCNPJ: String;
    FchBPe: String;
    FdhEvento: TDateTime;
    FtpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FverEvento: String;

    FDetEvento: TprocEvento_DetEvento;
    FRetInfEvento: TprocEvento_RetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: String              read FId             write FId;
    property cOrgao: Integer         read FcOrgao         write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb          write FtpAmb;
    property CNPJ: String            read FCNPJ           write FCNPJ;
    property chBPe: String           read FchBPe          write FchBPe;
    property dhEvento: TDateTime     read FdhEvento       write FdhEvento;
    property tpEvento: TpcnTpEvento  read FtpEvento       write FtpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
    property verEvento: String       read FverEvento      write FverEvento;

    property detEvento: TprocEvento_DetEvento read FDetEvento write FDetEvento;
    property RetinfEvento: TprocEvento_RetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TdocZipCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdocZipCollectionItem;
    procedure SetItem(Index: Integer; Value: TdocZipCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TdocZipCollectionItem;
    property Items[Index: Integer]: TdocZipCollectionItem read GetItem write SetItem; default;
  end;

  TdocZipCollectionItem = class(TCollectionItem)
  private
    // Atributos do resumo da BPe ou Evento
    FNSU: String;
    Fschema: TSchemaBPe;

    // A propriedade InfZip contem a informação Resumida ou documento fiscal
    // eletrônico Compactado no padrão gZip
    FInfZip: String;

    // Resumos e Processamento de Eventos Descompactados
    FresBPe: TresBPe;
    FresEvento: TresEvento;
    FprocEvento: TprocEvento;

    // XML do Resumo ou Documento descompactado
    FXML: String;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property NSU: String             read FNSU        write FNSU;
    property schema: TSchemaBPe      read Fschema     write Fschema;
    property InfZip: String          read FInfZip     write FInfZip;
    property resBPe: TresBPe         read FresBPe     write FresBPe;
    property resEvento: TresEvento   read FresEvento  write FresEvento;
    property procEvento: TprocEvento read FprocEvento write FprocEvento;
    property XML: String             read FXML        write FXML;
  end;

  TRetDistDFeInt = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FultNSU: String;
    FmaxNSU: String;
    FXML: AnsiString;
    FdocZip: TdocZipCollection;

    procedure SetdocZip(const Value: TdocZipCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    function LerXMLFromFile(CaminhoArquivo: String): Boolean;
  published
    property Leitor: TLeitor           read FLeitor   write FLeitor;
    property versao: String            read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente   read FtpAmb    write FtpAmb;
    property verAplic: String          read FverAplic write FverAplic;
    property cStat: Integer            read FcStat    write FcStat;
    property xMotivo: String           read FxMotivo  write FxMotivo;
    property dhResp: TDateTime         read FdhResp   write FdhResp;
    property ultNSU: String            read FultNSU   write FultNSU;
    property maxNSU: String            read FmaxNSU   write FmaxNSU;
    property docZip: TdocZipCollection read FdocZip   write SetdocZip;
    property XML: AnsiString           read FXML      write FXML;
  end;

implementation

uses 
  pcnAuxiliar,
  ACBrUtil, pcnGerador;

{ TprocEvento_DetEvento }

constructor TprocEvento_DetEvento.Create(AOwner: TprocEvento);
begin
  CTe  := TDetEventoCTe.Create;
  emit := TDetEventoEmit.Create;
end;

destructor TprocEvento_DetEvento.Destroy;
begin
  CTe.Free;
  emit.Free;
  inherited;
end;

{ TprocEvento }

constructor TprocEvento.Create;
begin
  FdetEvento    := TprocEvento_detEvento.Create(Self);
  FRetInfEvento := TprocEvento_RetInfEvento.Create;
end;

destructor TprocEvento.Destroy;
begin
  FdetEvento.Free;
  FRetInfEvento.Free;
  inherited;
end;

{ TdocZipCollection }

function TdocZipCollection.Add: TdocZipCollectionItem;
begin
  Result := TdocZipCollectionItem(inherited Add);
  Result.create;
end;

constructor TdocZipCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TdocZipCollectionItem);
end;

function TdocZipCollection.GetItem(Index: Integer): TdocZipCollectionItem;
begin
  Result := TdocZipCollectionItem(inherited GetItem(Index));
end;

procedure TdocZipCollection.SetItem(Index: Integer;
  Value: TdocZipCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdocZipCollectionItem }

constructor TdocZipCollectionItem.Create;
begin
  FresBPe     := TresBPe.Create;
  FresEvento  := TresEvento.Create;
  FprocEvento := TprocEvento.Create;
end;

destructor TdocZipCollectionItem.Destroy;
begin
  FresBPe.Free;
  FresEvento.Free;
  FprocEvento.Free;
  inherited;
end;

{ TRetDistDFeInt }

constructor TRetDistDFeInt.Create;
begin
  FLeitor := TLeitor.Create;
  FdocZip := TdocZipCollection.Create(Self);
end;

destructor TRetDistDFeInt.Destroy;
begin
  FLeitor.Free;
  FdocZip.Free;
  inherited;
end;

procedure TRetDistDFeInt.SetdocZip(const Value: TdocZipCollection);
begin
  FdocZip := Value;
end;

function TRetDistDFeInt.LerXml: Boolean;
var
  ok: Boolean;
  i: Integer;
  StrAux, StrDecod: AnsiString;
  oLeitorInfZip: TLeitor;
begin
  Result := False;

  try
    FXML := Self.Leitor.Arquivo;

    if (Leitor.rExtrai(1, 'retDistDFeInt') <> '') then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      FdhResp   := Leitor.rCampo(tcDatHor, 'dhResp');
      FultNSU   := Leitor.rCampo(tcStr, 'ultNSU');
      FmaxNSU   := Leitor.rCampo(tcStr, 'maxNSU');

      i := 0;
      while Leitor.rExtrai(2, 'docZip', '', i + 1) <> '' do
      begin
        FdocZip.Add;
        FdocZip.Items[i].FNSU   := Leitor.rAtributo('NSU');
        FdocZip.Items[i].schema := StrToSchemaBPe(ok, Leitor.rAtributo('schema'));

        StrAux := RetornarConteudoEntre(Leitor.Grupo, '>', '</docZip');
        FdocZip.Items[i].FInfZip := StrAux;
        StrDecod := UnZip(DecodeBase64(StrAux));

        oLeitorInfZip := TLeitor.Create;
        try
          oLeitorInfZip.Arquivo := StrDecod;

          if (oLeitorInfZip.rExtrai(1, 'resBPe') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FresBPe.chBPe    := oLeitorInfZip.rCampo(tcStr, 'chBPe');
            FdocZip.Items[i].FresBPe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');

            if FdocZip.Items[i].FresBPe.FCNPJCPF = '' then
              FdocZip.Items[i].FresBPe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresBPe.FxNome    := ParseText(oLeitorInfZip.rCampo(tcStr, 'xNome'));
            FdocZip.Items[i].FresBPe.FIE       := oLeitorInfZip.rCampo(tcStr, 'IE');
            FdocZip.Items[i].FresBPe.FdhEmi    := oLeitorInfZip.rCampo(tcDatHor, 'dhEmi');
            FdocZip.Items[i].FresBPe.FtpBPe    := StrToTpBPe(ok, oLeitorInfZip.rCampo(tcStr, 'tpBPe'));
            FdocZip.Items[i].FresBPe.FvNF      := oLeitorInfZip.rCampo(tcDe2, 'vNF');
            FdocZip.Items[i].FresBPe.FdigVal   := oLeitorInfZip.rCampo(tcStr, 'digVal');
            FdocZip.Items[i].FresBPe.FdhRecbto := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresBPe.FnProt    := oLeitorInfZip.rCampo(tcStr, 'nProt');
            FdocZip.Items[i].FresBPe.FcSitBPe  := StrToSituacaoDFe(ok, oLeitorInfZip.rCampo(tcStr, 'cSitBPe'));
          end;

          if (oLeitorInfZip.rExtrai(1, 'resEvento') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FresEvento.FcOrgao  := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
            FdocZip.Items[i].FresEvento.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');

            if FdocZip.Items[i].FresEvento.FCNPJCPF = '' then
              FdocZip.Items[i].FresEvento.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresEvento.chBPe       := oLeitorInfZip.rCampo(tcStr, 'chBPe');
            FdocZip.Items[i].FresEvento.FdhEvento   := oLeitorInfZip.rCampo(tcDatHor, 'dhEvento');
            FdocZip.Items[i].FresEvento.FtpEvento   := StrToTpEvento(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'));
            FdocZip.Items[i].FresEvento.FnSeqEvento := oLeitorInfZip.rCampo(tcInt, 'nSeqEvento');
            FdocZip.Items[i].FresEvento.FxEvento    := ParseText(oLeitorInfZip.rCampo(tcStr, 'xEvento'));
            FdocZip.Items[i].FresEvento.FdhRecbto   := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresEvento.FnProt      := oLeitorInfZip.rCampo(tcStr, 'nProt');
          end;

          if (oLeitorInfZip.rExtrai(1, 'BPeProc') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            oLeitorInfZip.rExtrai(1, 'infBPe');
            FdocZip.Items[i].FresBPe.chBPe := copy(oLeitorInfZip.Grupo, pos('Id="BPe', oLeitorInfZip.Grupo) + 7, 44);

            oLeitorInfZip.rExtrai(1, 'emit');
            FdocZip.Items[i].FresBPe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');
            if FdocZip.Items[i].FresBPe.FCNPJCPF = '' then
              FdocZip.Items[i].FresBPe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresBPe.FxNome := ParseText(oLeitorInfZip.rCampo(tcStr, 'xNome'));
            FdocZip.Items[i].FresBPe.FIE    := oLeitorInfZip.rCampo(tcStr, 'IE');

            oLeitorInfZip.rExtrai(1, 'ide');
            FdocZip.Items[i].FresBPe.FdhEmi := oLeitorInfZip.rCampo(tcDatHor, 'dhEmi');

            FdocZip.Items[i].FresBPe.FtpBPe := StrToTpBPe(ok, oLeitorInfZip.rCampo(tcStr, 'tpBPe'));

            oLeitorInfZip.rExtrai(1, 'total');
            FdocZip.Items[i].FresBPe.FvNF := oLeitorInfZip.rCampo(tcDe2, 'vNF');

            oLeitorInfZip.rExtrai(1, 'infProt');
            FdocZip.Items[i].FresBPe.digVal    := oLeitorInfZip.rCampo(tcStr, 'digVal');
            FdocZip.Items[i].FresBPe.FdhRecbto := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresBPe.FnProt    := oLeitorInfZip.rCampo(tcStr, 'nProt');

            case oLeitorInfZip.rCampo(tcInt, 'cStat') of
              100: FdocZip.Items[i].FresBPe.FcSitBPe := snAutorizado;
              101: FdocZip.Items[i].FresBPe.FcSitBPe := snCancelado;
              110: FdocZip.Items[i].FresBPe.FcSitBPe := snDenegado;
            end;
          end;

          if (oLeitorInfZip.rExtrai(1, 'procEventoBPe') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FprocEvento.FId         := oLeitorInfZip.rAtributo('Id');
            FdocZip.Items[i].FprocEvento.FcOrgao     := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
            FdocZip.Items[i].FprocEvento.FtpAmb      := StrToTpAmb(ok, oLeitorInfZip.rCampo(tcStr, 'tpAmb'));
            FdocZip.Items[i].FprocEvento.FCNPJ       := oLeitorInfZip.rCampo(tcStr, 'CNPJ');
            FdocZip.Items[i].FprocEvento.FchBPe      := oLeitorInfZip.rCampo(tcStr, 'chBPe');
            FdocZip.Items[i].FprocEvento.FdhEvento   := oLeitorInfZip.rCampo(tcDatHor, 'dhEvento');
            FdocZip.Items[i].FprocEvento.FtpEvento   := StrToTpEvento(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'));
            FdocZip.Items[i].FprocEvento.FnSeqEvento := oLeitorInfZip.rCampo(tcInt, 'nSeqEvento');
            FdocZip.Items[i].FprocEvento.FverEvento  := oLeitorInfZip.rCampo(tcStr, 'verEvento');

            if (oLeitorInfZip.rExtrai(2, 'detEvento') <> '') then
            begin
              FdocZip.Items[i].FprocEvento.detEvento.FVersao     := oLeitorInfZip.rAtributo('versao');
              FdocZip.Items[i].FprocEvento.detEvento.FnProt      := oLeitorInfZip.rCampo(tcStr, 'nProt');
              FdocZip.Items[i].FprocEvento.detEvento.FxJust      := ParseText(oLeitorInfZip.rCampo(tcStr, 'xJust'));
              FdocZip.Items[i].FprocEvento.detEvento.FxCorrecao  := ParseText(oLeitorInfZip.rCampo(tcStr, 'xCorrecao'));
              FdocZip.Items[i].FprocEvento.detEvento.FDescEvento := ParseText(oLeitorInfZip.rCampo(tcStr, 'descEvento'));

              if (oLeitorInfZip.rExtrai(3, 'CTe') <> '') then
              begin
                FdocZip.Items[i].FprocEvento.detEvento.FCTe.FchCTe    := oLeitorInfZip.rCampo(tcStr, 'chCTe');
                FdocZip.Items[i].FprocEvento.detEvento.FCTe.Fmodal    := StrToTpModal(ok, oLeitorInfZip.rCampo(tcStr, 'modal'));
                FdocZip.Items[i].FprocEvento.detEvento.FCTe.FdhEmi    := oLeitorInfZip.rCampo(tcDatHor, 'dhEmi');
                FdocZip.Items[i].FprocEvento.detEvento.FCTe.FnProt    := oLeitorInfZip.rCampo(tcStr, 'nProt');
                FdocZip.Items[i].FprocEvento.detEvento.FCTe.FdhRecbto := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
              end;

              if (oLeitorInfZip.rExtrai(3, 'emit') <> '') then
              begin
                FdocZip.Items[i].FprocEvento.detEvento.Femit.FCNPJ  := oLeitorInfZip.rCampo(tcStr, 'CNPJ');
                FdocZip.Items[i].FprocEvento.detEvento.Femit.FIE    := oLeitorInfZip.rCampo(tcStr, 'IE');
                FdocZip.Items[i].FprocEvento.detEvento.Femit.FxNome := ParseText(oLeitorInfZip.rCampo(tcStr, 'xNome'));
              end;
            end;

            if (oLeitorInfZip.rExtrai(2, 'retEvento') <> '') then
            begin
              FdocZip.Items[i].FprocEvento.RetinfEvento.FId          := oLeitorInfZip.rAtributo('Id');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FtpAmb       := StrToTpAmb(ok, oLeitorInfZip.rCampo(tcStr, 'tpAmb'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FverAplic    := oLeitorInfZip.rCampo(tcStr, 'verAplic');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FcOrgao      := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FcStat       := oLeitorInfZip.rCampo(tcInt, 'cStat');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FxMotivo     := ParseText(oLeitorInfZip.rCampo(tcStr, 'xMotivo'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FchBPe       := oLeitorInfZip.rCampo(tcStr, 'chBPe');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FtpEvento    := StrToTpEvento(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FxEvento     := ParseText(oLeitorInfZip.rCampo(tcStr, 'xEvento'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FnSeqEvento  := oLeitorInfZip.rCampo(tcInt, 'nSeqEvento');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FCNPJDest    := oLeitorInfZip.rCampo(tcStr, 'CNPJDest');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FdhRegEvento := oLeitorInfZip.rCampo(tcDatHor, 'dhRegEvento');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FnProt       := oLeitorInfZip.rCampo(tcStr, 'nProt');
            end;
          end;
        finally
          FreeAndNil(oLeitorInfZip);
        end;

        inc(i);
      end;

      Result := True;
    end;
  except
    on e : Exception do
    begin
//      result := False;
      Raise Exception.Create(e.Message);
    end;
  end;
end;

function TRetDistDFeInt.LerXMLFromFile(CaminhoArquivo: String): Boolean;
var
  ArqDist: TStringList;
begin
  ArqDist := TStringList.Create;
  try
     ArqDist.LoadFromFile(CaminhoArquivo);

     Self.Leitor.Arquivo := ArqDist.Text;

     Result := LerXml;
  finally
     ArqDist.Free;
  end;
end;

end.

