{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Claudio Roberto e             }
{                              Alessandro Yamasaki                             }
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

{******************************************************************************
|* Historico
|*
|* 12/12/2010: Isaque Pinheiro e Claudio Roberto
|*  - Criação e distribuição da Primeira Versao
|* 10/01/2011: Alessandro Yamasaki
|*  - Implementação ou atualização dos campos conforme
|*    Anexo Único do Ato Declaratório Executivo Cofis nº 034, de 2010 (Atualizado pelo ADE Cofis nº 37, de 2010)
|*    - A010 < WriteRegistroA010 > - REGISTRO A010: IDENTIFICAÇÃO DO ESTABELECIMENTO
|*    - A100 < WriteRegistroA100 > - REGISTRO A100: DOCUMENTO - NOTA FISCAL DE SERVIÇO
|*    - A111 < WriteRegistroA111 > - REGISTRO A111: PROCESSO REFERENCIADO
|*    - A120 < WriteRegistroA120 > - REGISTRO A120: INFORMAÇÃO COMPLEMENTAR - OPERAÇÕES DE IMPORTAÇÃO
|*    - A170 < WriteRegistroA170 > - REGISTRO A170: COMPLEMENTO DO DOCUMENTO - ITENS DO DOCUMENTO
*******************************************************************************}

unit ACBrEPCBloco_A_Class;

interface

uses ACBrSped, ACBrEPCBloco_A, ACBrEPCBlocos, ACBrEPCBloco_0_Class;

type
  /// TBloco_A - Abertura, Identificação e Referências

  { TBloco_A }

  TBloco_A = class(TACBrSPED)
  private
    FRegistroA001: TRegistroA001;      /// BLOCO 0 - Registro0001
    FRegistroA990: TRegistroA990;      /// BLOCO 0 - Registro0990

    FRegistroA010Count: Integer;
    FRegistroA100Count: Integer;
    FRegistroA110Count: Integer;
    FRegistroA111Count: Integer;
    FRegistroA120Count: Integer;
    FRegistroA170Count: Integer;
    FBloco_0: TBloco_0;

    procedure WriteRegistroA010(RegA001: TRegistroA001);
    procedure WriteRegistroA100(RegA010: TRegistroA010);
    procedure WriteRegistroA110(RegA100: TRegistroA100);
    procedure WriteRegistroA111(RegA100: TRegistroA100);
    procedure WriteRegistroA120(RegA100: TRegistroA100);
    procedure WriteRegistroA170(RegA100: TRegistroA100);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;          /// Create
    destructor  Destroy; override; /// Destroy

    procedure LimpaRegistros; override;

    function RegistroA001New: TRegistroA001;
    function RegistroA010New: TRegistroA010;
    function RegistroA100New: TRegistroA100;
    function RegistroA110New: TRegistroA110;
    function RegistroA111New: TRegistroA111;
    function RegistroA120New: TRegistroA120;
    function RegistroA170New: TRegistroA170;

    procedure WriteRegistroA001 ;
    procedure WriteRegistroA990 ;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;    
    property RegistroA001: TRegistroA001 read FRegistroA001 write FRegistroA001;
    property RegistroA990: TRegistroA990 read FRegistroA990 write FRegistroA990;

    property RegistroA010Count: Integer read FRegistroA010Count write FRegistroA010Count;
    property RegistroA100Count: Integer read FRegistroA100Count write FRegistroA100Count;
    property RegistroA110Count: Integer read FRegistroA110Count write FRegistroA110Count;
    property RegistroA111Count: Integer read FRegistroA111Count write FRegistroA111Count;
    property RegistroA120Count: Integer read FRegistroA120Count write FRegistroA120Count;
    property RegistroA170Count: Integer read FRegistroA170Count write FRegistroA170Count;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_A }

constructor TBloco_A.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_A.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_A.CriaRegistros;
begin
  FRegistroA001           := TRegistroA001.Create;
  FRegistroA990           := TRegistroA990.Create;

  FRegistroA010Count      := 0;
  FRegistroA100Count      := 0;
  FRegistroA110Count      := 0;
  FRegistroA120Count      := 0;
  FRegistroA170Count      := 0;

  FRegistroA990.QTD_LIN_A := 0;
end;

procedure TBloco_A.LiberaRegistros;
begin
  FRegistroA001.Free;
  FRegistroA990.Free;
end;

procedure TBloco_A.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_A.RegistroA001New: TRegistroA001;
begin
   Result := FRegistroA001;
end;

function TBloco_A.RegistroA010New: TRegistroA010;
begin
   Result := FRegistroA001.RegistroA010.New;
end;

function TBloco_A.RegistroA100New: TRegistroA100;
var
   A010Count : integer;
begin
   A010Count := FRegistroA001.RegistroA010.Count -1;
   //
   Result    := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.New;
end;

function TBloco_A.RegistroA110New: TRegistroA110;
var
    A010Count : integer;
    A100Count : integer;
begin
   A010Count := FRegistroA001.RegistroA010.Count -1;
   A100Count := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Count -1;
   //
   Result    := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Items[A100Count].RegistroA110.New;
end;

function TBloco_A.RegistroA111New: TRegistroA111;
  var
    A010Count : integer;
    A100Count : integer;
begin
   A010Count := FRegistroA001.RegistroA010.Count -1;
   A100Count := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Count -1;
   //
   Result    := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Items[A100Count].RegistroA111.New;
end;

function TBloco_A.RegistroA120New: TRegistroA120;
  var
    A010Count : integer;
    A100Count : integer;
begin
   A010Count := FRegistroA001.RegistroA010.Count -1;
   A100Count := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Count -1;
   //
   Result    := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Items[A100Count].RegistroA120.New;
end;

function TBloco_A.RegistroA170New: TRegistroA170;
var
    A010Count : integer;
    A100Count : integer;
begin
   A010Count := FRegistroA001.RegistroA010.Count -1;
   A100Count := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Count -1;
   //
   Result    := FRegistroA001.RegistroA010.Items[A010Count].RegistroA100.Items[A100Count].RegistroA170.New;
end;

procedure TBloco_A.WriteRegistroA001 ;
begin
  if Assigned(FRegistroA001) then
  begin
     if (RegistroA990.QTD_LIN_A = 0) then  // Ja gravou o A001?
     begin
        with FRegistroA001 do
        begin
           Add( LFill( 'A001' ) +
                LFill( Integer(IND_MOV), 0 ) ) ;
        end;
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
     end;
     if FRegistroA001.IND_MOV = imComDados then
     begin
       WriteRegistroA010(FRegistroA001) ;
     end;
  end;
end;

procedure TBloco_A.WriteRegistroA010(RegA001: TRegistroA001) ;
var
   intFor : Integer;
begin
  if Assigned(RegA001.RegistroA010) then
  begin
    if (FRegistroA010Count<RegA001.RegistroA010.Count) then // Algum A010 ainda nao gravado?
    begin
      for intFor := FRegistroA010Count to RegA001.RegistroA010.Count - 1 do
      begin
        with RegA001.RegistroA010.Items[intFor] do
        begin
          Check(funChecaCNPJ(CNPJ), '(A-010) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);

          Add( LFill('A010') +
               LFill(CNPJ, 14) ) ;
        end;

        // Registros FILHOS
        WriteRegistroA100( RegA001.RegistroA010.Items[intFor] );
        //
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
      end;
      // Variavél para armazenar a quantidade de registro do tipo.
      FRegistroA010Count := FRegistroA010Count + RegA001.RegistroA010.Count;
    end
    else // apenas gravar os registros FILHOS do ultimo A010 existente
    begin
      // Registros FILHOS
      WriteRegistroA100( RegA001.RegistroA010.Items[FRegistroA010Count-1] );
    end;
  end;
end;

procedure TBloco_A.WriteRegistroA100(RegA010: TRegistroA010) ;
  var
    intFor      : integer;
    strIND_OPER : String;
    strIND_EMIT : String;
    strCOD_SIT  : String;
    strIND_PGTO : String;
    booNFCancelada : Boolean;
begin
  if Assigned(RegA010.RegistroA100) then
  begin
     for intFor := 0 to RegA010.RegistroA100.Count - 1 do
     begin
        with RegA010.RegistroA100.Items[intFor] do
        begin
          //Check(NOME <> '', '(A-100) DOCUMENTO: O nome do XXX é obrigatório!');

          case IND_OPER of
            itoContratado  : strIND_OPER := '0';
            itoPrestado    : strIND_OPER := '1';
          end;
          case IND_EMIT of
            iedfProprio    : strIND_EMIT := '0';
            iedfTerceiro   : strIND_EMIT := '1';
          end;
          case COD_SIT of
            sdfRegular     : strCOD_SIT  := '00';
            sdfCancelado   : strCOD_SIT  := '02';
          end;

          /// Tratamento NFs canceladas 02 - 19-ago-2011
          if Pos(strCOD_SIT,'02') > 0 then
          begin
            //COD_PART       := ''; Correção Mário Mesquita - programador Criare Informática
            CHV_NFSE       := '';
            DT_DOC         := 0;
            DT_EXE_SERV    := 0;
            IND_PGTO       := tpNenhum;
            booNFCancelada := true
          end
          else
            booNFCancelada := false;

          case IND_PGTO of
            tpVista        : strIND_PGTO := '0';
            tpPrazo        : strIND_PGTO := '1';
            tpSemPagamento : strIND_PGTO := '9';
            tpNenhum       : strIND_PGTO := '';
          end;

          Add( LFill('A100')                             +
               LFill( strIND_OPER )                      +
               LFill( strIND_EMIT )                      +
               LFill( COD_PART )                         +
               LFill( strCOD_SIT )                       +
               LFill( SER )                              +
               LFill( SUB )                              +
               LFill( NUM_DOC )                          +
               LFill( CHV_NFSE )                         +
               LFill( DT_DOC )                           +
               LFill( DT_EXE_SERV )                      +
               LFill( VL_DOC,0,2,booNFCancelada )        +
               LFill( strIND_PGTO )                      +
               LFill( VL_DESC,0,2,booNFCancelada )       +
               LFill( VL_BC_PIS,0,2,booNFCancelada )     +
               LFill( VL_PIS,0,2,booNFCancelada )        +
               LFill( VL_BC_COFINS,0,2,booNFCancelada )  +
               LFill( VL_COFINS,0,2,booNFCancelada )     +
               LFill( VL_PIS_RET,0,2,booNFCancelada )    +
               LFill( VL_COFINS_RET,0,2,booNFCancelada ) +
               LFill( VL_ISS,0,2,booNFCancelada ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroA110( RegA010.RegistroA100.Items[intFor] );
        WriteRegistroA111( RegA010.RegistroA100.Items[intFor] );
        WriteRegistroA120( RegA010.RegistroA100.Items[intFor] );
        WriteRegistroA170( RegA010.RegistroA100.Items[intFor] );
        ///
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroA100Count := FRegistroA100Count + RegA010.RegistroA100.Count;
     //
     RegA010.RegistroA100.Clear;
  end;
end;

procedure TBloco_A.WriteRegistroA110(RegA100: TRegistroA100) ;
var
    intFor : integer;
begin
  if Assigned(RegA100.RegistroA110) then
  begin
    for intFor := 0 to RegA100.RegistroA110.Count - 1 do
    begin
      with RegA100.RegistroA110.Items[intFor] do
      begin
        Add( LFill('A110')  +
             LFill(COD_INF) +
             LFill(TXT_COMPL) ) ;
        //
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
      end;
   end;
   // Variavél para armazenar a quantidade de registro do tipo.
   FRegistroA110Count := FRegistroA110Count + RegA100.RegistroA110.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA111(RegA100: TRegistroA100) ;
  var
    intFor      : integer;
    strIND_PROC : String;
begin
  if Assigned(RegA100.RegistroA111) then
  begin
    for intFor := 0 to RegA100.RegistroA111.Count - 1 do
    begin
      with RegA100.RegistroA111.Items[intFor] do
      begin
        case IND_PROC of
          opJusticaFederal : strIND_PROC := '1';
          opSecexRFB       : strIND_PROC := '3';
          opOutros         : strIND_PROC := '9';
          opNenhum         : strIND_PROC := '';
        end;

        Add( LFill('A111')   +
             LFill(NUM_PROC) +
             LFill(strIND_PROC) ) ;
      end;

      RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroA111Count := FRegistroA111Count + RegA100.RegistroA111.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA120(RegA100: TRegistroA100) ;
  var
    intFor          : integer;
    strLOC_EXE_SERV : String;
begin
  if Assigned(RegA100.RegistroA120) then
  begin
     for intFor := 0 to RegA100.RegistroA120.Count - 1 do
     begin
        with RegA100.RegistroA120.Items[intFor] do
        begin
          case LOC_EXE_SERV of
            lesExecutPais     : strLOC_EXE_SERV := '0';
            lesExecutExterior : strLOC_EXE_SERV := '1';
          end;

          Add( LFill('A120')              +
               LFill( VL_TOT_SERV,0,2 )   +
               LFill( VL_BC_PIS,0,2 )     +
               LFill( VL_PIS_IMP,0,2 )    +
               LFill( DT_PAG_PIS)         +
               LFill( VL_BC_COFINS,0,2 )  +
               LFill( VL_COFINS_IMP,0,2 ) +
               LFill( DT_PAG_COFINS)      +
               LFill( strLOC_EXE_SERV) );
        end;
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroA120Count := FRegistroA120Count + RegA100.RegistroA120.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA170(RegA100: TRegistroA100) ;
  var
    intFor           : integer;
    strIND_ORIG_CRED : String;
    strNAT_BC_CRED   : String;    
begin
  if Assigned(RegA100.RegistroA170) then
  begin
    for intFor := 0 to RegA100.RegistroA170.Count - 1 do
    begin
      with RegA100.RegistroA170.Items[intFor] do
      begin
        //Check(Reg0001.Registro0190.LocalizaRegistro(UNID), '(0-0190) UNIDADE MEDIDA: A unidade de medida "%s" foi duplicada na lista de registros 0190!', [UNID]);

        case IND_ORIG_CRED of
            opcVazio          : strIND_ORIG_CRED := '';
            opcMercadoInterno : strIND_ORIG_CRED := '0';
            opcImportacao     : strIND_ORIG_CRED := '1';
        else
            strIND_ORIG_CRED := '';
        end;

        case NAT_BC_CRED of
          bccVazio                         : strNAT_BC_CRED := '';
          bccAqBensRevenda                 : strNAT_BC_CRED := '01';
          bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
          bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
          bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
          bccAluguelPredios                : strNAT_BC_CRED := '05';
          bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
          bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
          bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
          bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
          bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
          bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
          bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
          bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
          bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
          bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
          bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
          bccAtPresServ                    : strNAT_BC_CRED := '17';
          bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
        else
          strNAT_BC_CRED := '';
        end;

        Add( LFill('A170')             +
             LFill(NUM_ITEM, 0)        +
             LFill(COD_ITEM)           +
             LFill(DESCR_COMPL)        +
             LFill( VL_ITEM,0,2 )      +
             LFill( VL_DESC,0,2 )      +
             LFill( strNAT_BC_CRED)    +
             LFill(strIND_ORIG_CRED)   +
             LFill( CstPisToStr(CST_PIS))        +
             LFill( VL_BC_PIS,0,2 )    +
             DFill( ALIQ_PIS,4 )     +
             LFill( VL_PIS,0,2 )       +
             LFill( CstCofinsToStr(CST_COFINS))  +
             LFill( VL_BC_COFINS,0,2 ) +
             DFill( ALIQ_COFINS,4 )  +
             LFill( VL_COFINS,0,2 )    +
             LFill(COD_CTA)            +
             LFill(COD_CCUS) ) ;

        end;
        RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroA170Count := FRegistroA170Count + RegA100.RegistroA170.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA990 ;
begin
  if Assigned(RegistroA990) then
  begin
     with RegistroA990 do
     begin
       QTD_LIN_A := QTD_LIN_A + 1;
       ///
       Add( LFill('A990') +
            LFill(QTD_LIN_A,0) );
     end;
  end;
end;

end.
