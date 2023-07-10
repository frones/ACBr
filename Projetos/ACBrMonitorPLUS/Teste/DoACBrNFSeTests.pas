unit DoACBrNFSeTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, IniFiles, ACBrTests.Util, ACBrMonitorTestConsts, ACBrNFSeX,
  ACBrNFSeXConversao, ACBrMonitorConfig, DoACBrNFSeUnit, CmdUnit;

type

{ TDoACBrNFSeTest }

TDoACBrNFSeTest = class(TTestCase)
private
  FMonitorConfig: TMonitorConfig;
  FObjetoDono: TACBrObjetoNFSe;
  FACBrNFSeX: TACBrNFSeX;
  FCmd: TACBrCmd;
public
  procedure SetUp; override;
  procedure TearDown; override;
  procedure ConfigurarNFSeX;
published
  procedure AdicionarRPSCaminhoIni;
  procedure AdicionarRPSConteudoIni;
  procedure LerIniNFSeCaminhoIni;
  procedure LerIniNFSeConteudoIni;
end;

implementation

{ TDoACBrNFSeTest }

procedure TDoACBrNFSeTest.SetUp;
begin
  inherited SetUp;
  FMonitorConfig := TMonitorConfig.Create(MONITORINICONFIG);
  FACBrNFSeX := TACBrNFSeX.Create(nil);
  ConfigurarNFSeX;
  FObjetoDono := TACBrObjetoNFSe.Create(FMonitorConfig, FACBrNFSeX);
  FCmd := TACBrCmd.Create;
end;

procedure TDoACBrNFSeTest.TearDown;
begin
  FreeAndNil(FObjetoDono);
  FreeAndNil(FMonitorConfig);
  FreeAndNil(FACBrNFSeX);
  FreeAndNil(FCmd);

  inherited TearDown;
end;

procedure TDoACBrNFSeTest.ConfigurarNFSeX;
var
  IniConfig: TMemIniFile;
begin
  IniConfig := TMemIniFile.Create(MONITORINICONFIG);
  try
    if not IniConfig.SectionExists('NFSE') then
       raise Exception.Create('Arquivo MonitorConfig.ini em branco, preencha o arquivo!');

    FACBrNFSeX.Configuracoes.Geral.Emitente.WSUser := IniConfig.ReadString('NFSe', 'Usuario', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.WSSenha := IniConfig.ReadString('NFSe', 'Senha', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.WSChaveAcesso := IniConfig.ReadString('NFSe', 'ChaveAutenticacao', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.WSFraseSecr := IniConfig.ReadString('NFSe', 'FraseSecreta', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.CNPJ := IniConfig.ReadString('NFSe', 'CNPJEmitente', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.InscMun := IniConfig.ReadString('NFSe', 'IMEmitente', '');
    FACBrNFSeX.Configuracoes.Geral.Emitente.RazSocial := IniConfig.ReadString('NFSe', 'NomeEmitente', '');
    FACBrNFSeX.Configuracoes.Geral.MontarPathSchema := IniConfig.ReadBool('NFSe', 'MontarAutoPathSchema', True);
    FACBrNFSeX.Configuracoes.Geral.ConsultaAposCancelar := IniConfig.ReadBool('NFSe', 'ConsultarLoteAposEnvio', True);
    FACBrNFSeX.Configuracoes.Geral.ConsultaLoteAposEnvio := IniConfig.ReadBool('NFSe', 'ConsultarAposCancelar', True);
    FACBrNFSeX.Configuracoes.Geral.CNPJPrefeitura := IniConfig.ReadString('NFSe', 'CNPJPrefeitura', '');
    FACBrNFSeX.Configuracoes.Geral.LayoutNFSe := IfThen<TLayoutNFSe>(IniConfig.ReadInteger('NFSe', 'LayoutProvedor', 0)=0, lnfsProvedor, lnfsPadraoNacionalv1) ;
    FACBrNFSeX.Configuracoes.Geral.CodigoMunicipio := IniConfig.ReadInteger('NFSe', 'CodigoMunicipio', 0);
  finally
    FreeAndNil(IniConfig);
  end;
end;

procedure TDoACBrNFSeTest.AdicionarRPSCaminhoIni;
var
  FParameters: TStringList;
begin
  FParameters := TStringList.Create;
  try
    FParameters.Clear;
    FParameters.Add(NFSEINI);
    FParameters.Add('1');

    FACBrNFSeX.NotasFiscais.Clear;

    FCmd.Comando := 'NFSe.AdicionarRPS("'+FParameters.Strings[0]+'",' + FParameters.Strings[1]+')';

    FObjetoDono.Executar(FCmd);
    Check(FCmd.Params(0) = FParameters.Strings[0], 'Params(0) diferente do esperado!'+
                                                   sLineBreak + 'Esperado:'+
                                                   sLineBreak + FParameters.Strings[0] +
                                                   SLineBreak + 'Recebido:'+
                                                   SLineBreak + FCmd.Params(0));

    Check(FCmd.Params(1) = FParameters.Strings[1], 'Params(1) diferente do esperado!'+
                                                   sLineBreak + 'Esperado:'+
                                                   sLineBreak + FParameters.Strings[1] +
                                                   SLineBreak + 'Recebido:'+
                                                   SLineBreak + FCmd.Params(1));
    Check(FCmd.Resposta = 'Total RPS Adicionados= 1', 'Esperado:Total RPS Adicionados= 1|Recebido:'+FCmd.Resposta);

  finally
    FreeAndNil(FParameters);
  end;
end;

procedure TDoACBrNFSeTest.AdicionarRPSConteudoIni;
var
  IniNFSe: TStringList;
  FParameters: TStringList;
begin
  FACBrNFSeX.NotasFiscais.Clear;

  IniNFSe := TStringList.Create;
  FParameters := TStringList.Create;
  try
    IniNFSe.LoadFromFile(NFSEINI);

    FParameters.Add(IniNFSe.Text);
    FParameters.Add('1');

    FCmd.Comando := 'NFSe.AdicionarRPS("'+FParameters.Strings[0]+'",'+FParameters.Strings[1]+')';

    FObjetoDono.Executar(FCmd);
    Check(FCmd.Params(0) = FParameters.Strings[0], 'Params(0) diferente do esperado!'+
                                                   sLineBreak + 'Esperado:'+
                                                   sLineBreak + FParameters.Strings[0] +
                                                   SLineBreak + 'Recebido:'+
                                                   SLineBreak + FCmd.Params(0));

    Check(FCmd.Params(1) = FParameters.Strings[1], 'Params(1) diferente do esperado!'+
                                                   sLineBreak + 'Esperado:'+
                                                   sLineBreak + FParameters.Strings[1] +
                                                   SLineBreak + 'Recebido:'+
                                                   SLineBreak + FCmd.Params(1));
    Check(FCmd.Resposta = 'Total RPS Adicionados= 1', 'Esperado:Total RPS Adicionados= 1|Recebido:'+FCmd.Resposta);
  finally
    FreeAndNil(IniNFSe);
    FreeAndNil(FParameters);
  end;
end;

procedure TDoACBrNFSeTest.LerIniNFSeCaminhoIni;
begin
  FCmd.Comando := 'NFSe.AdicionarRPS("'+NFSEINI+'", 1)';

  FACBrNFSeX.NotasFiscais.Clear;
  FACBrNFSeX.NotasFiscais.LoadFromIni(FCmd.Params(0));

  CheckTrue(FACBrNFSeX.NotasFiscais.Count = 1, 'LerIniNFSe não carregou NFSe');
end;

procedure TDoACBrNFSeTest.LerIniNFSeConteudoIni;
var
  IniNFSe: TStringList;
begin
  IniNFSe := TStringList.Create;
  try
    IniNFSe.LoadFromFile(NFSEINI);

    FCmd.Comando := 'NFSe.AdicionarRPS("'+IniNFSe.Text+'", 1)';
    FACBrNFSeX.NotasFiscais.LoadFromIni(FCmd.Params(0));

    CheckTrue(FACBrNFSeX.NotasFiscais.Count = 1, 'LerIniNFSe não carregou NFSe');
  finally
    FreeAndNil(IniNFSe);
  end;
end;

initialization
  _RegisterTest('ACBrMonitor.DoACBrNFSeUnit', TDoACBrNFSeTest);

end.

