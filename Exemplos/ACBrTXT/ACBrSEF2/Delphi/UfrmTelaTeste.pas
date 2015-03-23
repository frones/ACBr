unit UfrmTelaTeste;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, inifiles, ACBrSEF2, ACBrSEF2Conversao,
  ACBrSEF2_eDoc_BlocoC,  ACBrSef2_BlocoE;

type
  TfrmTelaTeste = class(TForm)
    stat1: TStatusBar;
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    bvl1: TBevel;
    lbl1: TLabel;
    bvl2: TBevel;
    lbl2: TLabel;
    btnB_0: TButton;
    btnB_E: TButton;
    btnSalva_SEF2: TButton;
    btnB_C: TButton;
    btnSalva_eDoc: TButton;
    btnB_G: TButton;
    btnB_F: TButton;
    btnB_H: TButton;
    rgConteudoArquivo: TRadioGroup;
    dlgSave1: TSaveDialog;
    lbl20: TLabel;
    cbbIND_ED: TComboBox;
    lbl21: TLabel;
    cbbIND_ARQ: TComboBox;
    lbl22: TLabel;
    cbbPRF_ISS: TComboBox;
    lbl23: TLabel;
    cbbPRF_ICMS: TComboBox;
    lbl24: TLabel;
    cbbPRF_RIDF: TComboBox;
    lbl25: TLabel;
    cbbPRF_RUDF: TComboBox;
    lbl26: TLabel;
    cbbPRF_LMC: TComboBox;
    lbl27: TLabel;
    cbbPRF_RV: TComboBox;
    lbl28: TLabel;
    cbbPRF_RI: TComboBox;
    lbl29: TLabel;
    cbbIND_EC: TComboBox;
    lbl30: TLabel;
    cbbIND_ISS: TComboBox;
    lbl31: TLabel;
    cbbIND_RT: TComboBox;
    lbl32: TLabel;
    cbbIND_ST: TComboBox;
    lbl33: TLabel;
    cbbIND_AT: TComboBox;
    lbl34: TLabel;
    cbbIND_IPI: TComboBox;
    lbl35: TLabel;
    cbbIND_RI: TComboBox;
    lbl36: TLabel;
    cbbIND_ICMS: TComboBox;
    btnDefinirPath: TButton;
    ACBrSEF2: TACBrSEF2;
    procedure FormCreate(Sender: TObject);
    procedure btnB_0Click(Sender: TObject);
    procedure btnB_EClick(Sender: TObject);
    procedure btnB_CClick(Sender: TObject);
    procedure btnSalva_SEF2Click(Sender: TObject);
    procedure btnSalva_eDocClick(Sender: TObject);
    procedure rgConteudoArquivoClick(Sender: TObject);
    procedure btnDefinirPathClick(Sender: TObject);
  private
    { Private declarations }
    procedure GeraArq20;
    procedure GeraArq60;
    procedure GravaINI;
    procedure LeINI;
  public
    { Public declarations }
  end;

var
  frmTelaTeste: TfrmTelaTeste;
  SEF2 : TACBrSEF2;
implementation

uses ACBrSEF2_eDoc_BlocoC_Class,  ACBrSEF2_Bloco0_1,  ACBrSEF2_Bloco0,
   ACBrSEF2_BlocoE_1;


{$R *.dfm}

procedure TfrmTelaTeste.GravaINI;
var cINI : TIniFile;
I : Integer;
begin
  cINI := TIniFile.Create(ExtractFilePath(Application.ExeName)+'REG_030.ini');
  for I := 0 to frmTelaTeste.ComponentCount-1 do
  begin
    if frmTelaTeste.Components[I].ClassType = TComboBox then
    begin
      if TComboBox(frmTelaTeste.Components[I]).Tag = 5 then
        cINI.WriteString('eDOC',TComboBox(frmTelaTeste.Components[I]).Name,TComboBox(frmTelaTeste.Components[I]).Text);
    end;
  end;
end;

procedure TfrmTelaTeste.LeINI;
var cINI : TIniFile;
I : Integer;
begin
  if Not FileExists(ExtractFilePath(Application.ExeName)+'REG_030.ini') then
    Exit;

  cINI := TIniFile.Create(ExtractFilePath(Application.ExeName)+'REG_030.ini');
  for I := 0 to frmTelaTeste.ComponentCount-1 do
  begin
    if frmTelaTeste.Components[I].ClassType = TComboBox then
    begin
      if TComboBox(frmTelaTeste.Components[I]).Tag = 5 then
        TComboBox(frmTelaTeste.Components[I]).Text := cINI.ReadString('eDOC',TComboBox(frmTelaTeste.Components[I]).Name,'');
    end;
  end;
end;
procedure TfrmTelaTeste.FormCreate(Sender: TObject);
begin
  LeInI;
end;

procedure TfrmTelaTeste.btnB_0Click(Sender: TObject);
var
  wIndiceOpcao: Integer;
begin
  GravaINI;

  ACBrSEF2.DT_INI := StrToDate('01/01/2013');
  ACBrSEF2.DT_FIN := StrToDate('31/01/2013');

  ACBrSEF2.Delimitador := '|';
  ACBrSEF2.CurMascara  := '#0.00';
  ACBrSEF2.IniciaGeracao;

  with ACBrSEF2.Bloco_0 do
  begin
     with Registro0000New do
     begin
        NOME_EMPR := 'MERCANTIL SANTANA LTDA';
        CNPJ      := '04219316000120';
        UF        := 'PE';
        IE        := '027771776';
        COD_MUN   := '2613909';
        IM        := '';
        SUFRAMA   := '';
        FANTASIA  := 'MERCANTIL SANTANA';
        CPF       := '';
        NIRE      := '';
        PAIS      := 'Brasil';
        COD_VER   := vlVersao2000;
        COD_FIN   := raOriginal;

        if rgConteudoArquivo.ItemIndex = 0 then
           COD_CTD := caLancOpResultFiscal
        else if rgConteudoArquivo.ItemIndex = 1 then
           COD_CTD := caExtratodocfiscais;

        with Registro0001New do
        begin
           IND_MOV := icContConteudo;

           with Registro0005New do
           begin
              NOME_RESP := 'MARIA IZALRA SILVA LIMA';
              COD_ASSIN := qaDiretor;
              CPF_RESP  := '08320888468';
              CEP       := '56900000';
              ENDERECO  := 'AV:AFONSO MAGALHAES';
              NUM       := '998';
              COMPL     := '';
              BAIRRO    := 'SAO CRISTOVAO';
              CEP_CP    := '';
              CP        := '';
              FONE      := '8738311732';
              FAX       := '8738311732';
              EMAIL     := 'mercantilsantana@gruposantanape.com.br';
           end;

           if ACBrSEF2.TipoArquivo = aSEF then
           begin
              with Registro0025New do
              begin
                 COD_BF_ICMS := bfNenhum;
                 COD_BF_ISS  := '0';
              end;
           end;

           with Registro0030New do
           begin
              wIndiceOpcao := cbbIND_ED.ItemIndex;
              case wIndiceOpcao of
                 0: IND_ED := entDigitacao;
                 1: IND_ED := entImportacao;
                 2: IND_ED := entValidacao;
              end;

              wIndiceOpcao := cbbIND_ARQ.ItemIndex;
              case wIndiceOpcao of
                 0: IND_ARQ := arqOriginal;
                 1: IND_ARQ := arqTranscricaoEmissaoPropria;
                 2: IND_ARQ := arqTranscricaoEmissaoTerceiros;
                 3: IND_ARQ := arqTrancricaoDigitalizacao;
                 4: IND_ARQ := arqTranscricaoEmissaoEquipEspecilizado;
                 5: IND_ARQ := arqLivrosResultadosObrigacoes;
                 6: IND_ARQ := arqLivroMapaControles;
                 7: IND_ARQ := arqGuiasInfEconomicasFiscais;
                 8: IND_ARQ := arqLivrosContabilidade;
                 9: IND_ARQ := arqExtratoDocumentos;
              end;

              wIndiceOpcao := cbbPRF_ISS.ItemIndex;
              case wIndiceOpcao of
                0: PRF_ISS  := impSimRegimeSimplificado;
                1: PRF_ICMS := impSimRegimeIntegral;
              end;

              wIndiceOpcao := cbbPRF_RUDF.ItemIndex;
              if wIndiceOpcao = 0 then
                 PRF_RUDF :=  exSim
              else
                 PRF_RUDF :=  exNao;

              wIndiceOpcao := cbbPRF_LMC.ItemIndex;
              if wIndiceOpcao = 0 then
                 PRF_LMC :=  exSim
              else
                 PRF_LMC :=  exNao;

              wIndiceOpcao := cbbPRF_RV.ItemIndex;
              if wIndiceOpcao = 0 then
                 PRF_RV :=  exSim
              else
                 PRF_RV :=  exNao;

              wIndiceOpcao := cbbPRF_RI.ItemIndex;
              if wIndiceOpcao = 0 then
                 PRF_RI :=  exSim
              else
                 PRF_RI :=  exNao;

              wIndiceOpcao := cbbIND_EC.ItemIndex;
              case wIndiceOpcao of
                 0: IND_EC := esCompletaArquivo;
                 1: IND_EC := esCompletaPapel;
                 2: IND_EC := esSimplificadaArquivo;
                 3: IND_EC := esSimplificadaPapel;
                 4: IND_EC := esLivroCaixaArquivo;
                 5: IND_EC := esLivroCaixaPapel;
                 6: IND_EC := esNaoObrigado;
              end;

              wIndiceOpcao := cbbIND_ISS.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_ISS :=  exSim
              else
                 IND_ISS :=  exNao;

              wIndiceOpcao := cbbIND_RT.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_RT :=  exSim
              else
                 IND_RT :=  exNao;

              wIndiceOpcao := cbbIND_ICMS.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_ICMS :=  exSim
              else
                 IND_ICMS :=  exNao;

              wIndiceOpcao := cbbIND_ST.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_ST :=  exSim
              else
                 IND_ST :=  exNao;

              wIndiceOpcao := cbbIND_AT.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_AT :=  exSim
              else
                 IND_AT :=  exNao;

              wIndiceOpcao := cbbIND_IPI.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_IPI :=  exSim
              else
                 IND_IPI :=  exNao;

              wIndiceOpcao := cbbIND_RI.ItemIndex;
              if wIndiceOpcao = 0 then
                 IND_RI :=  exSim
              else
                 IND_RI :=  exNao;

           end;

           with Registro0100New do
           begin
              NOME      := 'JOSE CARMELO DE FARIAS';
              COD_ASSIN := qaContador;
              CNPJ      := '12957646000140';
              CRC       := 'PE005673O2';
              CEP       := '56900000';
              ENDERECO  := 'NOME DA RUA DO CONTABILISTA';
              NUM       := '236';
              COMPL     := 'COMPLEMENTO';
              BAIRRO    := 'BAIRRO';
              UF        := 'PE';
              COD_MUN   := 2613909;
              CEP_CP    := 0;
              CP        := 0;//CAIXA POSTAL
              FONE      := '8738312020';
              FAX       := '8738312020';
              EMAIL     := 'acene@acenecontabilidade.com.br';
           end;

           with Registro0150New do
           begin
              COD_PART := '1';
              NOME     := 'TESTE PARTICIPANTE 1';
              COD_PAIS := '01058';
              CNPJ     := '05499080000195';
              CPF      := '';
              UF       := 'PE';
              IE       := '';
              IE_ST    := '';
              COD_MUN  := 2613909;
              IM       := '';
              SUFRAMA  := '';
           end;

           with Registro0150New do
           begin
              COD_PART := '2';
              NOME     := 'BEMATECH S/A';
              COD_PAIS := '01058';
              CNPJ     := '82373077001810';
              CPF      := '';
              UF       := 'SP';
              IE       := '';
              IE_ST    := '';
              COD_MUN  := 3513801;
              IM       := '';
              SUFRAMA  := '';
           end;

           with Registro0150New do
           begin
              COD_PART := '3';
              NOME     := 'TESTE PARTICIPANTE 2';
              COD_PAIS := '01058';
              CNPJ     := '05553541000160';
              CPF      := '';
              UF       := 'PE';
              IE       := '';
              IE_ST    := '';
              COD_MUN  := 3513801;
              IM       := '';
              SUFRAMA  := '';
           end;

           with Registro0200New do
           begin
              COD_ITEM   := '1';
              DESCR_ITEM := 'PRODUTOS PARA TESTE';
              COD_GEN    := '10';
              COD_LST    := '';
           end;

           with Registro0400New do
           begin
              COD_NAT   := '1102';
              DESCR_NAT := 'ENTRADA DE MERCADORIAS';
              COP       := 'EA10';
           end;

           with Registro0400New do
           begin
              COD_NAT   := '5102';
              DESCR_NAT := 'SAIDA DE MERCADORIAS';
              COP       := 'SP10';
           end;
        end;
     end;
  end;

  ACBrSEF2.WriteBloco_0;
end;

procedure TfrmTelaTeste.GeraArq20();
var
  wRegistroE020: TRegistroSEFE020;
  wRegistroE025: TRegistroSEFE025;
begin
   wRegistroE020:= ACBrSEF2.Bloco_E.RegistroE020New;

   with wRegistroE020 do
   begin
      IND_OPER     := SefioEntrada;
      IND_EMIT     := SefiePropria;
      COD_PART     := '1';
      COD_MOD      := SrefNF;
      COD_SIT      := SefcsEmissaonormal;
      SER          := '1';
      NUM_DOC      := 12;
      CHV_NFE      := '';
      DT_EMIS      := StrToDate('01/01/2013');
      DT_DOC       := StrToDate('01/01/2013');
      COD_NAT      := '1102';
      COP          := '1102';
      NUM_LCTO     := '0';
      IND_PGTO     := SefipAVista;
      VL_CONT      := 100;
      VL_OP_ISS    :=0;
      VL_BC_ICMS   := 100;
      VL_ICMS      := 17;
      VL_ICMS_ST   := 0;
      VL_ST_E      := 0;
      VL_ST_S      := 0;
      VL_AT        := 0;
      VL_ISNT_ICMS := 0;
      VL_OUT_ICMS  := 0;
      VL_BC_IPI    := 0;
      VL_IPI       := 0;
      VL_ISNT_IPI  := 0;
      VL_OUT_IPI   := 0;
      COD_INF_OBS  := '';

      wRegistroE025:= wRegistroE020.RegistroE025.New(wRegistroE020);

      with wRegistroE025 do
      begin
         VL_CONT_P      := 100;
         VL_OP_ISS_P    := 0;
         CFOP           := '1102';
         VL_BC_ICMS_P   := 100;
         ALIQ_ICMS      := 17;
         VL_ICMS_P      := 17;
         VL_BC_ST_P     := 0;
         VL_ICMS_ST_P   := 0;
         VL_ISNT_ICMS_P := 0;
         VL_BC_IPI_P    := 0;
         VL_IPI_P       := 0;
         VL_ISNT_IPI_P  := 0;
         VL_OUT_IPI_P   := 0;
         IND_PETR       := 0;
         IND_IMUN       := 0;
      end;
   end;

   wRegistroE020:= ACBrSEF2.Bloco_E.RegistroE020New;

   with wRegistroE020 do
   begin
      IND_OPER     := SefioSaida;
      IND_EMIT     := SefiePropria;
      COD_PART     := '3';
      COD_MOD      := SrefNF;
      COD_SIT      := SefcsEmissaonormal;
      SER          := '1';
      NUM_DOC      := 14;
      CHV_NFE      := '';
      DT_EMIS      := StrToDate('01/01/2013');
      DT_DOC       := StrToDate('01/01/2013');
      COD_NAT      := '6102';
      COP          := '6102';
      NUM_LCTO     := '0';
      IND_PGTO     := SefipAVista;
      VL_CONT      := 100;
      VL_OP_ISS    := 0;
      VL_BC_ICMS   := 0;
      VL_ICMS      := 0;
      VL_ICMS_ST   := 20;
      VL_ST_E      := 0;
      VL_ST_S      := 20;
      VL_AT        := 0;
      VL_ISNT_ICMS := 0;
      VL_OUT_ICMS  := 0;
      VL_BC_IPI    := 0;
      VL_IPI       := 0;
      VL_ISNT_IPI  := 0;
      VL_OUT_IPI   := 0;
      COD_INF_OBS  := '';

      wRegistroE025:= wRegistroE020.RegistroE025.New(wRegistroE020);

      with wRegistroE025 do
      begin
         VL_CONT_P      := 100;
         VL_OP_ISS_P    := 0;
         CFOP           := '6102';
         VL_BC_ICMS_P   := 0;
         ALIQ_ICMS      := 0;
         VL_ICMS_P      := 0;
         VL_BC_ST_P     := 100;
         VL_ICMS_ST_P   := 20;
         VL_ISNT_ICMS_P := 0;
         VL_BC_IPI_P    := 0;
         VL_IPI_P       := 0;
         VL_ISNT_IPI_P  := 0;
         VL_OUT_IPI_P   := 0;
         IND_PETR       := 0;
         IND_IMUN       := 0;
      end;
   end;

   wRegistroE020:= ACBrSEF2.Bloco_E.RegistroE020New;

   with wRegistroE020 do
   begin
      IND_OPER     := SefioSaida;
      IND_EMIT     := SefiePropria;
      COD_PART     := '3';
      COD_MOD      := SrefNF;
      COD_SIT      := SefcsEmissaonormal;
      SER          := '1';
      NUM_DOC      := 13;
      CHV_NFE      := '';
      DT_EMIS      := StrToDate('01/01/2013');
      DT_DOC       := StrToDate('01/01/2013');
      COD_NAT      := '5102';
      COP          := '5102';
      NUM_LCTO     := '0';
      IND_PGTO     := SefipAVista;
      VL_CONT      := 100;
      VL_OP_ISS    := 0;
      VL_BC_ICMS   := 0;
      VL_ICMS      := 0;
      VL_ICMS_ST   := 0;
      VL_ST_E      := 0;
      VL_ST_S      := 0;
      VL_AT        := 0;
      VL_ISNT_ICMS := 10;
      VL_OUT_ICMS  := 0;
      VL_BC_IPI    := 0;
      VL_IPI       := 0;
      VL_ISNT_IPI  := 0;
      VL_OUT_IPI   := 0;
      COD_INF_OBS  := '';

      wRegistroE025:= wRegistroE020.RegistroE025.New(wRegistroE020);

      with wRegistroE025 do
      begin
         VL_CONT_P      := 100;
         VL_OP_ISS_P    := 0;
         CFOP           := '5102';
         VL_BC_ICMS_P   := 0;
         ALIQ_ICMS      := 0;
         VL_ICMS_P      := 0;
         VL_BC_ST_P     := 0;
         VL_ICMS_ST_P   := 0;
         VL_ISNT_ICMS_P := 10;
         VL_BC_IPI_P    := 0;
         VL_IPI_P       := 0;
         VL_ISNT_IPI_P  := 0;
         VL_OUT_IPI_P   := 0;
         IND_PETR       := 0;
         IND_IMUN       := 0;
      end;
   end;
end;

procedure TfrmTelaTeste.GeraArq60;
var
  wRegistroE060: TRegistroSEFE060;
  wRegistroE065: TRegistroSEFE065;
begin

   wRegistroE060:= ACBrSEF2.Bloco_E.RegistroE060New;

   with wRegistroE060 do
   begin
      COD_MOD      := SrefCCF;
      ECF_CX       := 1;
      ECF_FAB      := 'BE091110100011282077';
      CRO          := 10;
      CRZ          := 10;
      DT_DOC       := StrToDate('01/01/2013');
      NUM_DOC_INI  := 1;
      NUM_DOC_FIN  := 10;
      GT_INI       := 0;
      GT_FIN       := 100;
      VL_BRT       := 100;
      VL_CANC_ICMS := 0;
      VL_DESC_ICMS := 0;
      VL_ACMO_ICMS := 0;
      VL_OP_ISS    := 0;
      VL_LIQ       := 100;
      VL_BC_ICMS   := 100;
      VL_ICMS      := 17;
      VL_ISN       := 0;
      VL_NT        := 0;
      VL_ST        := 0;
      COD_INF_OBS  := '';

      wRegistroE065:= wRegistroE060.RegistroE065.New(wRegistroE060);

      with wRegistroE065 do
      begin
         CFOP         := '5102';
         VL_BC_ICMS_P := 100;
         ALIQ_ICMS    := 17;
         VL_ICMS_P    := 17;
         IND_IMUN     := 0;
      end;
   end;
end;


procedure TfrmTelaTeste.btnB_EClick(Sender: TObject);
var
  wRegistroE080: TRegistroSEFE080;
  wRegistroE085: TRegistroSEFE085;
begin
   ACBrSEF2.Bloco_E.RegistroE001.IND_DAD :=  idDocEntrSaiAjuste;

   GeraArq20;
   GeraArq60;

   wRegistroE080:= ACBrSEF2.Bloco_E.RegistroE080New;

   with wRegistroE080 do
   begin
      IND_TOT      := 1;
      COD_MOD      := SrefNFVCCVC;
      NUM_MR       := 1;
      DT_DOC       := StrToDate('01/01/2013');
      VL_BRT       := 100;
      VL_CANC_ICMS := 0;
      VL_DESC_ICMS := 0;
      VL_ACMO_ICMS := 0;
      VL_OP_ISS    := 0;
      COP          := '5102';
      NUM_LCTO     := '1';
      VL_CONT      := 100;
      VL_BC_ICMS   := 100;
      VL_ICMS      := 17;
      VL_ISNT_ICMS := 0;
      VL_ST        := 0;
      IND_OBS      := 0;
   end;

   wRegistroE085:= wRegistroE080.RegistroE085.New(wRegistroE080);

   with wRegistroE085 do
   begin
      VL_CONT_P      := 100;
      VL_OP_ISS_P    := 0;
      VL_BC_ICMS_P   := 100;
      ALIQ_ICMS      := 17;
      VL_ISNT_ICMS_P := 0;
      VL_ST_P        := 0;
      IND_IMUN       := 0;
   end;
end;

procedure TfrmTelaTeste.btnSalva_SEF2Click(Sender: TObject);
begin
  ACBrSEF2.SaveFileTXT;
end;

procedure TfrmTelaTeste.btnB_CClick(Sender: TObject);
var
  INotas, NNotas: Integer;
  IItens: Integer;
  wRegistroC020: TRegistroSEFC020;
  wRegistroC300: TRegistroSEFC300;
begin
  NNotas := 2;

  with ACBrSEF2.Bloco_C do
  begin
     with RegistroC001New do
     begin
       IND_DAD := icContConteudo;

       for INotas := 1 to NNotas do
       begin
          wRegistroC020:= RegistroC020New;

          with wRegistroC020 do
          begin
             IND_OPER    := SefioEntrada;
             IND_EMIT    := SefieTerceiros;
             COD_PART    := '1';
             COD_MOD     := SrefNFe;
             COD_SIT     := SefcsEmissaonormal;
             SER         := '1';
             NUM_DOC     := 72 + (INotas - 1);
             CHV_NFE     := '13120310172378000126550010000010361466480726';
             DT_EMIS     := StrToDate('01/01/2013');
             DT_DOC      := StrToDate('01/01/2013');
             COD_NAT     := '1102';
             IND_PGTO    := SefipAVista;
             VL_DOC      := 100;
             VL_DESC     := 0;
             VL_ACMO     := 0;
             VL_MERC     := 100;
             VL_FRT      := 0;
             VL_SEG      := 0;
             VL_OUT_DA   := 0;
             VL_OP_ISS   := 0;
             VL_BC_ICMS  := 0;
             VL_ICMS     := 17;
             VL_BC_ST    := 0;
             VL_ICMS_ST  := 0;
             VL_AT       := 0;
             VL_IPI      := 0;
             COD_INF_OBS := '';

             //LINHA C300: ITENS DO DOCUMENTO
             for IItens := 1 to 10 do
             begin
                wRegistroC300 := wRegistroC020.RegistrosC300.New(wRegistroC020);

                with wRegistroC300 do
                begin
                   NUM_ITEM     := IItens;
                   COD_ITEM     := '1';
                   UNID         := 'CX';
                   VL_UNIT      := 100;
                   QTD          := 1;
                   VL_DESC_I    := 0;
                   VL_ACMO_I    := 0;
                   VL_ITEM      := 1;
                   COD_NCM      := '22021000';
                   CST          := '000';
                   CFOP         := 1102;
                   VL_BC_ICMS_I := 100;
                   ALIQ_ICMS    := 17;
                   VL_ICMS_I    := 17;
                   VL_BC_ST_I   := 0;
                   ALIQ_ST      := 0;
                   VL_ICMS_ST_I := 0;
                   VL_BC_IPI    := 0;
                   ALIQ_IPI     := 0;
                   VL_IPI_I     := 0;
                end;
             end;
          end;
       end;
    end;
  end;

  ACBrSEF2.WriteBloco_C(true);
end;

procedure TfrmTelaTeste.btnSalva_eDocClick(Sender: TObject);
begin
   ACBrSEF2.SaveFileTXT;
end;

procedure TfrmTelaTeste.rgConteudoArquivoClick(Sender: TObject);
begin
  if rgConteudoArquivo.ItemIndex = 0 then
  begin
    btnSalva_eDoc.Enabled := False;
    btnB_C.Enabled := False;
    btnB_E.Enabled := True;
    btnSalva_SEF2.Enabled := True;
    ACBrSEF2.TipoArquivo:= aSEF;
  end
  else
  begin
    btnB_E.Enabled := False;
    btnSalva_SEF2.Enabled := False;
    btnSalva_eDoc.Enabled := True;
    btnB_C.Enabled := True;
    ACBrSEF2.TipoArquivo:= aEDOC;
  end;
end;

procedure TfrmTelaTeste.btnDefinirPathClick(Sender: TObject);
var
  wPathCompleto: String;
begin
   wPathCompleto:= ExtractFilePath(Application.ExeName);
   ACBrSEF2.Path    := ExtractFilePath(wPathCompleto);
   ACBrSEF2.Arquivo := 'sef2.txt';
end;

end.
