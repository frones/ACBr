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

unit pcnRetDistDFeInt;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor, synacode;

type
  TresDFe               = class;
  TresEvento            = class;
  TprocEvento           = class;
  TdocZipCollection     = class;
  TdocZipCollectionItem = class;
  TRetDistDFeInt        = class;
  TitensAverbadosCollection     = class;
  TitensAverbadosCollectionItem = class;

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

  TitensAverbadosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TitensAverbadosCollectionItem;
    procedure SetItem(Index: Integer; Value: TitensAverbadosCollectionItem);
  public
    function New: TitensAverbadosCollectionItem;
    property Items[Index: Integer]: TitensAverbadosCollectionItem read GetItem write SetItem; default;
  end;

  TitensAverbadosCollectionItem = class(TObject)
  private
    FdhEmbarque: TDateTime;
    FdhAverbacao: TDateTime;
    FnDue: String;
    FnItem: Integer;
    FnItemDue: Integer;
    FqItem: Integer;
    FmotAlteracao: Integer;

  public
    property dhEmbarque: TDateTime  read FdhEmbarque   write FdhEmbarque;
    property dhAverbacao: TDateTime read FdhAverbacao  write FdhAverbacao;
    property nDue: String           read FnDue         write FnDue;
    property nItem: Integer         read FnItem        write FnItem;
    property nItemDue: Integer      read FnItemDue     write FnItemDue;
    property qItem: Integer         read FqItem        write FqItem;
    property motAlteracao: Integer  read FmotAlteracao write FmotAlteracao;
  end;

  TprocEvento_DetEvento = class
  private
    FVersao: String;
    FDescEvento: String;
    FnProt: String;
    FxJust: String;
    FxCorrecao: String;
    FtpAutor: Integer;
    FverAplic: String;

    FCTe: TDetEventoCTe;
    Femit: TDetEventoEmit;
    FitensAverbados: TitensAverbadosCollection;

    procedure SetitensAverbados(const Value: TitensAverbadosCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property versao: String     read FVersao     write FVersao;
    property descEvento: String read FDescEvento write FDescEvento;
    property nProt: String      read FnProt      write FnProt;
    property xJust: String      read FxJust      write FxJust;
    property xCorrecao: String  read FxCorrecao  write FxCorrecao;
    property tpAutor: Integer   read FtpAutor    write FtpAutor;
    property verAplic: String   read FverAplic   write FverAplic;

    property CTe: TDetEventoCTe   read FCTe  write FCTe;
    property emit: TDetEventoEmit read Femit write Femit;
    property itensAverbados: TitensAverbadosCollection read FitensAverbados write SetitensAverbados;
  end;

  TprocEvento_RetInfEvento = class
  private
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchDFe: String;
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
    // Alterado de chNFe/chCTe e chMDFe para chDFe (italo)
    property chDFe: String           read FchDFe       write FchDFe;
    property tpEvento: TpcnTpEvento  read FtpEvento    write FtpEvento;
    property xEvento: String         read FxEvento     write FxEvento;
    property nSeqEvento: Integer     read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String        read FCNPJDest    write FCNPJDest;
    property emailDest: String       read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer    read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime  read FdhRegEvento write FdhRegEvento;
    property nProt: String           read FnProt       write FnProt;
  end;

  // Alterado de resNFe/resCTe e resMDFe para resDFe (italo)
  TresDFe = class
  private
    FchDFe: String;
    FCNPJCPF: String;
    FxNome: String;
    FIE: String;
    FdhEmi: TDateTime;
    FtpNF: TpcnTipoNFe;
    FvNF: Currency;
    FdigVal: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FcSitDFe: TSituacaoDFe;
  public
    // Alterado de chNFe/chCTe e chMDFe para chDFe (italo)
    property chDFe: String            read FchDFe    write FchDFe;
    property CNPJCPF: String          read FCNPJCPF  write FCNPJCPF;
    property xNome: String            read FxNome    write FxNome;
    property IE: String               read FIE       write FIE;
    property dhEmi: TDateTime         read FdhEmi    write FdhEmi;
    property tpNF: TpcnTipoNFe        read FtpNF     write FtpNF;
    property vNF: Currency            read FvNF      write FvNF;
    property digVal: String           read FdigVal   write FdigVal;
    property dhRecbto: TDateTime      read FdhRecbto write FdhRecbto;
    property nProt: String            read FnProt    write FnProt;
  // Alterado de cSitNFe/cSitCTe e cSitMDFe para cSitDFe (italo)
    property cSitDFe: TSituacaoDFe    read FcSitDFe  write FcSitDFe;
  end;

  TresEvento = class
  private
    FcOrgao: Integer;
    FCNPJCPF: String;
    FchDFe: String;
    FdhEvento: TDateTime;
    FtpEvento: TpcnTpEvento;
    FnSeqEvento: ShortInt;
    FxEvento: String;
    FdhRecbto: TDateTime;
    FnProt: String;
  public
    property cOrgao: Integer        read FcOrgao     write FcOrgao;
    property CNPJCPF: String        read FCNPJCPF    write FCNPJCPF;
    // Alterado de chNFe/chCTe e chMDFe para chDFe (italo)
    property chDFe: String          read FchDFe      write FchDFe;
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
    FchDFe: String;
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
    // Alterado de chNFe/chCTe e chMDFe para chDFe (italo)
    property chDFe: String           read FchDFe          write FchDFe;
    property dhEvento: TDateTime     read FdhEvento       write FdhEvento;
    property tpEvento: TpcnTpEvento  read FtpEvento       write FtpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
    property verEvento: String       read FverEvento      write FverEvento;

    property detEvento: TprocEvento_DetEvento read FDetEvento write FDetEvento;
    property RetinfEvento: TprocEvento_RetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TdocZipCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdocZipCollectionItem;
    procedure SetItem(Index: Integer; Value: TdocZipCollectionItem);
  public
    function New: TdocZipCollectionItem;
    property Items[Index: Integer]: TdocZipCollectionItem read GetItem write SetItem; default;
  end;

  TdocZipCollectionItem = class(TObject)
  private
    // Atributos do resumo do DFe ou Evento
    FNSU: String;
    Fschema: TSchemaDFe;

    // A propriedade InfZip contem a informação Resumida ou documento fiscal
    // eletrônico Compactado no padrão gZip
    FInfZip: String;

    // Resumos e Processamento de Eventos Descompactados
    FresDFe: TresDFe;
    FresEvento: TresEvento;
    FprocEvento: TprocEvento;

    // XML do Resumo ou Documento descompactado
    FXML: String;
    FNomeArq: String;

  public
    constructor Create;
    destructor Destroy; override;

    property NSU: String             read FNSU        write FNSU;
    property schema: TSchemaDFe      read Fschema     write Fschema;
    property InfZip: String          read FInfZip     write FInfZip;
    // Alterado de resNFe/resCTe e resMDFe para resDFe (italo)
    property resDFe: TresDFe         read FresDFe     write FresDFe;
    property resEvento: TresEvento   read FresEvento  write FresEvento;
    property procEvento: TprocEvento read FprocEvento write FprocEvento;
    property XML: String             read FXML        write FXML;
    property NomeArq: String         read FNomeArq    write FNomeArq;
  end;

  TRetDistDFeInt = class
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
    FtpDFe: string;

    procedure SetdocZip(const Value: TdocZipCollection);
  public
    constructor Create(Const AtpDFe: String);
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXMLFromFile(Const CaminhoArquivo: String): Boolean;
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
  ACBrUtil;

{ TitensAverbadosCollection }

function TitensAverbadosCollection.GetItem(
  Index: Integer): TitensAverbadosCollectionItem;
begin
  Result := TitensAverbadosCollectionItem(inherited Items[Index]);
end;

function TitensAverbadosCollection.New: TitensAverbadosCollectionItem;
begin
  Result := TitensAverbadosCollectionItem.Create;
  Add(Result);
end;

procedure TitensAverbadosCollection.SetItem(Index: Integer;
  Value: TitensAverbadosCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TprocEvento_DetEvento }

constructor TprocEvento_DetEvento.Create;
begin
  inherited Create;

  FCTe  := TDetEventoCTe.Create;
  Femit := TDetEventoEmit.Create;
  FitensAverbados := TitensAverbadosCollection.Create();
end;

destructor TprocEvento_DetEvento.Destroy;
begin
  FCTe.Free;
  Femit.Free;
  FitensAverbados.Free;

  inherited;
end;

procedure TprocEvento_DetEvento.SetitensAverbados(
  const Value: TitensAverbadosCollection);
begin
  FitensAverbados := Value;
end;

{ TprocEvento }

constructor TprocEvento.Create;
begin
  inherited Create;
  FdetEvento    := TprocEvento_detEvento.Create;
  FRetInfEvento := TprocEvento_RetInfEvento.Create;
end;

destructor TprocEvento.Destroy;
begin
  FdetEvento.Free;
  FRetInfEvento.Free;

  inherited;
end;

{ TdocZipCollection }

function TdocZipCollection.GetItem(Index: Integer): TdocZipCollectionItem;
begin
  Result := TdocZipCollectionItem(inherited Items[Index]);
end;

procedure TdocZipCollection.SetItem(Index: Integer;
  Value: TdocZipCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdocZipCollection.New: TdocZipCollectionItem;
begin
  Result := TdocZipCollectionItem.Create;
  Add(Result);
end;

{ TdocZipCollectionItem }

constructor TdocZipCollectionItem.Create;
begin
  inherited Create;
  FresDFe     := TresDFe.Create;
  FresEvento  := TresEvento.Create;
  FprocEvento := TprocEvento.Create;
end;

destructor TdocZipCollectionItem.Destroy;
begin
  FresDFe.Free;
  FresEvento.Free;
  FprocEvento.Free;

  inherited;
end;

{ TRetDistDFeInt }

constructor TRetDistDFeInt.Create(Const AtpDFe: String);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FdocZip := TdocZipCollection.Create();

  FtpDFe := AtpDFe;
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

function TRetDistDFeInt.LerXml: boolean;
var
  ok: boolean;
  i, j: Integer;
  StrAux, StrDecod: AnsiString;
  oLeitorInfZip: TLeitor;
begin
  Result := False;

  try
    FXML := Self.Leitor.Arquivo;

    if (Leitor.rExtrai(1, 'retDistDFeInt') <> '') then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retDistDFeInt');
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
        FdocZip.New;
        FdocZip.Items[i].FNSU   := Leitor.rAtributo('NSU', 'docZip');
        FdocZip.Items[i].schema := StrToSchemaDFe(Leitor.rAtributo('schema', 'docZip'));

        StrAux := RetornarConteudoEntre(Leitor.Grupo, '>', '</docZip');
        FdocZip.Items[i].FInfZip := StrAux;
        StrDecod := UnZip(DecodeBase64(StrAux));

        oLeitorInfZip := TLeitor.Create;
        try
          oLeitorInfZip.Arquivo := StrDecod;

          if (oLeitorInfZip.rExtrai(1, 'res' + FtpDFe) <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FresDFe.chDFe := oLeitorInfZip.rCampo(tcStr, 'ch' + FtpDFe);

            FdocZip.Items[i].FresDFe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');

            if FdocZip.Items[i].FresDFe.FCNPJCPF = '' then
              FdocZip.Items[i].FresDFe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresDFe.FxNome    := ParseText(oLeitorInfZip.rCampo(tcStr, 'xNome'));
            FdocZip.Items[i].FresDFe.FIE       := oLeitorInfZip.rCampo(tcStr, 'IE');
            FdocZip.Items[i].FresDFe.FdhEmi    := oLeitorInfZip.rCampo(tcDatHor, 'dhEmi');
            FdocZip.Items[i].FresDFe.FtpNF     := StrToTpNF(ok, oLeitorInfZip.rCampo(tcStr, 'tpNF'));
            FdocZip.Items[i].FresDFe.FvNF      := oLeitorInfZip.rCampo(tcDe2, 'vNF');
            FdocZip.Items[i].FresDFe.FdigVal   := oLeitorInfZip.rCampo(tcStr, 'digVal');
            FdocZip.Items[i].FresDFe.FdhRecbto := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresDFe.FnProt    := oLeitorInfZip.rCampo(tcStr, 'nProt');
            FdocZip.Items[i].FresDFe.FcSitDFe  := StrToSituacaoDFe(ok, oLeitorInfZip.rCampo(tcStr, 'cSitNFe'));
          end;

          if (oLeitorInfZip.rExtrai(1, 'resEvento') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FresEvento.FcOrgao  := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
            FdocZip.Items[i].FresEvento.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');

            if FdocZip.Items[i].FresEvento.FCNPJCPF = '' then
              FdocZip.Items[i].FresEvento.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresEvento.chDFe := oLeitorInfZip.rCampo(tcStr, 'ch' + FtpDFe);

            FdocZip.Items[i].FresEvento.FdhEvento   := oLeitorInfZip.rCampo(tcDatHor, 'dhEvento');
            // É preciso identificar o DF-e para usar a função de conversão correta.
            FdocZip.Items[i].FresEvento.FtpEvento   := StrToTpEventoDFe(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'), FtpDFe);
            FdocZip.Items[i].FresEvento.FnSeqEvento := oLeitorInfZip.rCampo(tcInt, 'nSeqEvento');
            FdocZip.Items[i].FresEvento.FxEvento    := ParseText(oLeitorInfZip.rCampo(tcStr, 'xEvento'));
            FdocZip.Items[i].FresEvento.FdhRecbto   := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresEvento.FnProt      := oLeitorInfZip.rCampo(tcStr, 'nProt');
          end;

          if (oLeitorInfZip.rExtrai(1, LowerCase(FtpDFe) + 'Proc') <> '') or
             (oLeitorInfZip.rExtrai(1, LowerCase(FtpDFe) + 'OSProc') <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            // no caso do cte não é CTe e sim Cte.
            if FtpDFe = 'CTe' then
              oLeitorInfZip.rExtrai(1, 'infCte')
            else
              oLeitorInfZip.rExtrai(1, 'inf' + FtpDFe);

            FdocZip.Items[i].FresDFe.chDFe := copy(oLeitorInfZip.Grupo,
               pos('Id="' + FtpDFe, oLeitorInfZip.Grupo) + 4 + length(FtpDFe), 44);

            oLeitorInfZip.rExtrai(1, 'emit');
            FdocZip.Items[i].FresDFe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CNPJ');
            if FdocZip.Items[i].FresDFe.FCNPJCPF = '' then
              FdocZip.Items[i].FresDFe.FCNPJCPF := oLeitorInfZip.rCampo(tcStr, 'CPF');

            FdocZip.Items[i].FresDFe.FxNome := ParseText(oLeitorInfZip.rCampo(tcStr, 'xNome'));
            FdocZip.Items[i].FresDFe.FIE    := oLeitorInfZip.rCampo(tcStr, 'IE');

            oLeitorInfZip.rExtrai(1, 'ide');
            FdocZip.Items[i].FresDFe.FdhEmi := oLeitorInfZip.rCampo(tcDatHor, 'dhEmi');

            // Se for Versão 2.00 a data de emissão está em dEmi
            if FdocZip.Items[i].FresDFe.FdhEmi = 0 then
              FdocZip.Items[i].FresDFe.FdhEmi := oLeitorInfZip.rCampo(tcDat, 'dEmi');

            FdocZip.Items[i].FresDFe.FtpNF := StrToTpNF(ok, oLeitorInfZip.rCampo(tcStr, 'tpNF'));

            // Leitura do valor da nota fiscal - NF-e
            oLeitorInfZip.rExtrai(1, 'total');
            FdocZip.Items[i].FresDFe.FvNF := oLeitorInfZip.rCampo(tcDe2, 'vNF');

            // Leitura do valor total da prestação - CT-e
            if FdocZip.Items[i].FresDFe.FvNF = 0 then
            begin
              oLeitorInfZip.rExtrai(1, 'vPrest');
              FdocZip.Items[i].FresDFe.FvNF := oLeitorInfZip.rCampo(tcDe2, 'vTPrest');
            end;

            oLeitorInfZip.rExtrai(1, 'infProt');
            FdocZip.Items[i].FresDFe.digVal    := oLeitorInfZip.rCampo(tcStr, 'digVal');
            FdocZip.Items[i].FresDFe.FdhRecbto := oLeitorInfZip.rCampo(tcDatHor, 'dhRecbto');
            FdocZip.Items[i].FresDFe.FnProt    := oLeitorInfZip.rCampo(tcStr, 'nProt');

            case oLeitorInfZip.rCampo(tcInt, 'cStat') of
              100: FdocZip.Items[i].FresDFe.FcSitDFe := snAutorizado;
              101: FdocZip.Items[i].FresDFe.FcSitDFe := snCancelado;
              110: FdocZip.Items[i].FresDFe.FcSitDFe := snDenegado;
              132: FdocZip.Items[i].FresDFe.FcSitDFe := snEncerrado;
           end;
          end;

          if (oLeitorInfZip.rExtrai(1, 'procEvento' + FtpDFe) <> '') then
          begin
            FdocZip.Items[i].XML := InserirDeclaracaoXMLSeNecessario(oLeitorInfZip.Grupo);

            FdocZip.Items[i].FprocEvento.FId         := oLeitorInfZip.rAtributo('Id');
            FdocZip.Items[i].FprocEvento.FcOrgao     := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
            FdocZip.Items[i].FprocEvento.FtpAmb      := StrToTpAmb(ok, oLeitorInfZip.rCampo(tcStr, 'tpAmb'));
            FdocZip.Items[i].FprocEvento.FCNPJ       := oLeitorInfZip.rCampo(tcStr, 'CNPJ');
            FdocZip.Items[i].FprocEvento.FchDFe      := oLeitorInfZip.rCampo(tcStr, 'ch' + FtpDFe);
            FdocZip.Items[i].FprocEvento.FdhEvento   := oLeitorInfZip.rCampo(tcDatHor, 'dhEvento');
            // É preciso identificar o DF-e para usar a função de conversão correta.
            FdocZip.Items[i].FprocEvento.FtpEvento   := StrToTpEventoDFe(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'), FtpDFe);
            FdocZip.Items[i].FprocEvento.FnSeqEvento := oLeitorInfZip.rCampo(tcInt, 'nSeqEvento');
            FdocZip.Items[i].FprocEvento.FverEvento  := oLeitorInfZip.rCampo(tcStr, 'verEvento');

            if (oLeitorInfZip.rExtrai(2, 'detEvento') <> '') then
            begin
              FdocZip.Items[i].FprocEvento.detEvento.FVersao     := oLeitorInfZip.rAtributo('versao');
              FdocZip.Items[i].FprocEvento.detEvento.FnProt      := oLeitorInfZip.rCampo(tcStr, 'nProt');
              FdocZip.Items[i].FprocEvento.detEvento.FxJust      := ParseText(oLeitorInfZip.rCampo(tcStr, 'xJust'));
              FdocZip.Items[i].FprocEvento.detEvento.FxCorrecao  := ParseText(oLeitorInfZip.rCampo(tcStr, 'xCorrecao'));
              FdocZip.Items[i].FprocEvento.detEvento.FDescEvento := ParseText(oLeitorInfZip.rCampo(tcStr, 'descEvento'));
              FdocZip.Items[i].FprocEvento.detEvento.FtpAutor    := oLeitorInfZip.rCampo(tcInt, 'tpAutor');
              FdocZip.Items[i].FprocEvento.detEvento.FverAplic   := oLeitorInfZip.rCampo(tcStr, 'verAplic');

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

              j := 0;
              while Leitor.rExtrai(3, 'itensAverbados', '', j + 1) <> '' do
              begin
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.New;

                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FdhEmbarque   := oLeitorInfZip.rCampo(tcDatHor, 'dhEmbarque');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FdhAverbacao  := oLeitorInfZip.rCampo(tcDatHor, 'dhAverbacao');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FnDue         := oLeitorInfZip.rCampo(tcStr, 'nDue');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FnItem        := oLeitorInfZip.rCampo(tcInt, 'nItem');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FnItemDue     := oLeitorInfZip.rCampo(tcInt, 'nItemDue');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FqItem        := oLeitorInfZip.rCampo(tcInt, 'qItem');
                FdocZip.Items[i].FprocEvento.detEvento.FitensAverbados.Items[j].FmotAlteracao := oLeitorInfZip.rCampo(tcInt, 'motAlteracao');

                inc(j);
              end;
            end;

            if (oLeitorInfZip.rExtrai(2, 'retEvento') <> '') or
               (oLeitorInfZip.rExtrai(2, 'retEvento' + FtpDFe) <> '') then
            begin
              FdocZip.Items[i].FprocEvento.RetinfEvento.FId          := oLeitorInfZip.rAtributo('Id');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FtpAmb       := StrToTpAmb(ok, oLeitorInfZip.rCampo(tcStr, 'tpAmb'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FverAplic    := oLeitorInfZip.rCampo(tcStr, 'verAplic');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FcOrgao      := oLeitorInfZip.rCampo(tcInt, 'cOrgao');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FcStat       := oLeitorInfZip.rCampo(tcInt, 'cStat');
              FdocZip.Items[i].FprocEvento.RetinfEvento.FxMotivo     := ParseText(oLeitorInfZip.rCampo(tcStr, 'xMotivo'));
              FdocZip.Items[i].FprocEvento.RetinfEvento.FchDFe       := oLeitorInfZip.rCampo(tcStr, 'ch' + FtpDFe);
              // É preciso identificar o DF-e para usar a função de conversão correta.
              FdocZip.Items[i].FprocEvento.RetinfEvento.FtpEvento    := StrToTpEventoDFe(ok, oLeitorInfZip.rCampo(tcStr, 'tpEvento'), FtpDFe);
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

function TRetDistDFeInt.LerXMLFromFile(Const CaminhoArquivo: String): Boolean;
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

