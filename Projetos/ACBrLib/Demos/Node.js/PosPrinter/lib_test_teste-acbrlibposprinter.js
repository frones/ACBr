var ref = require('ref');
var ffi = require('ffi');

var tint = ref.refType('int');
var tlong = ref.refType('long');
var tchar = ref.refType('char *');
var tshort = ref.refType('short');
var tvoid = ref.refType('void');
var buflength = 256;

var eArqConfig = '/home/user/projeto_teste/acbrlib.ini';
var eChaveCrypt = '';
var dllACBrLibPosPrinter = '/home/user/projeto_teste/libacbrposprinter64.so';

var aINI = [
	{ section: 'Principal', key: 'TipoResposta', value: '2' },
	{ section: 'Principal', key: 'CodificacaoResposta', value: '0' },
	{ section: 'Principal', key: 'LogNivel', value: '4' },
	{ section: 'Principal', key: 'LogPath', value: '/home/user/projeto_teste/acbrlib.log' },
	// 
	{ section: 'Sistema', key: 'Nome', value: 'Navegador PDV' },
	{ section: 'Sistema', key: 'Versao', value: '4.0.0-beta' },
	{ section: 'Sistema', key: 'Data', value: '07/05/2020' },
	{ section: 'Sistema', key: 'Descricao', value: 'Navegador PDV' },
	//
	{ section: 'PosPrinter', key: 'ArqLog', value: '/home/user/projeto_teste/acbrlib_posprint.log' },
	{ section: 'PosPrinter', key: 'Modelo', value: '0' },
	{ section: 'PosPrinter', key: 'Porta', value: '/home/user/projeto_teste/impressora.log' },
	{ section: 'PosPrinter', key: 'PaginaDeCodigo', value: '2' },
	{ section: 'PosPrinter', key: 'ColunasFonteNormal', value: '48' },
	// { section: 'PosPrinter', key: 'EspacoEntreCupons', value: '0' },
	// { section: 'PosPrinter', key: 'LinhaEntreCupons', value: '0' },
	{ section: 'PosPrinter', key: 'CortaPapel', value: '0' },
	{ section: 'PosPrinter', key: 'TipoCorte', value: '0' },
	{ section: 'PosPrinter', key: 'TraduzirTags', value: '1' },
	{ section: 'PosPrinter', key: 'IgnorarTags', value: '0' },
	{ section: 'PosPrinter', key: 'LinhasBuffer', value: '0' },
	{ section: 'PosPrinter', key: 'ControlePorta', value: '0' },
	{ section: 'PosPrinter', key: 'VerificarImpressora', value: '0' },
]

var ACBrLibPosPrinter = ffi.Library(dllACBrLibPosPrinter, {
	// Métodos da Biblioteca
	POS_Inicializar: ['int', ['string', 'string']],
	POS_Finalizar: ['int', ['void']],
	POS_UltimoRetorno: ['int', [tchar, tint]],
	POS_Nome: ['int', [tchar, tint]],
	POS_Versao: ['int', [tchar, tint]],
	// Métodos de Configuração
	POS_ConfigLer: ['int', ['string']],
	POS_ConfigGravar: ['int', ['string']],
	POS_ConfigLerValor: ['int', ['string', 'string', tchar, tint]],
	POS_ConfigGravarValor: ['int', ['string', 'string', 'string']],
	// Métodos PosPrinter
	POS_Ativar: ['int', ['void']],
	POS_Desativar: ['int', ['void']],
	POS_TxRx: ['int', ['string', 'byte', 'int', 'bool', tchar, tint]],
	POS_Zerar: ['int', ['void']],
	POS_InicializarPos: ['int', ['void']],
	POS_Reset: ['int', ['void']],
	POS_PularLinhas: ['int', ['int']],
	POS_CortarPapel: ['int', ['bool']],
	POS_AbrirGaveta: ['int', ['void']],
	POS_LerInfoImpressora: ['int', [tchar, tint]],
	POS_LerStatusImpressora: ['int', ['int', tlong]],
	POS_RetornarTags: ['int', ['bool', tchar, tint]],
	// Métodos de Impressão
	POS_Imprimir: ['int', ['string', 'bool', 'bool', 'bool', 'int']],
	POS_ImprimirLinha: ['int', ['string']],
	POS_ImprimirCmd: ['int', ['string']],
	POS_ImprimirTags: ['int', ['void']],
});

var cbx = function (callback) {
	if (callback !== undefined && typeof (callback) === "function") {
		return callback;
	}
	return function () {
		console.log("-- callback is empty --");
	};
}

var sTrim = function (valor) {
	return (typeof (valor) === 'string' ? valor.replace(/\0+$/, '').replace(/^\s+|\s+$/gm, '') : valor);
}

var getResultMessage = function (result) {
	console.log('getResultMessage(' + result + ')');
	switch (result) {
		case -1:
			return 'Houve falhas na inicialização da biblioteca.';
		case -2:
			return 'Houve falhas na finalização da biblioteca.';
		case -3:
			return 'Houve erro ao ler a configuração informada.';
		case -5:
			return 'Não foi possível localizar o arquivo de configuração informado.';
		case -6:
			return 'Não foi possível encontrar o diretório do arquivo de configuração.';
		case -10:
			return 'Houve falhas na execução do método.';
		default:
			return 'Desconhecido (#' + result + ')';
	}
}

var CheckResult = function (Result, aloc_sMensagem, aloc_esTamanho, cb, iniLength) {
	try {

		if (Result !== 0)
			return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

		var sMensagem = sTrim(aloc_sMensagem.toString());
		var esTamanho = aloc_esTamanho.deref();

		console.log('CheckResult <<< sMensagem = ' + JSON.stringify(sMensagem));
		console.log('CheckResult <<< esTamanho = ' + JSON.stringify(esTamanho));

		var loopping = true;
		if (esTamanho < buflength) {
			loopping = false;
		} else {
			if (sMensagem.length < esTamanho && sMensagem.length > buflength) {
				loopping = false;
			} else {
				if (sMensagem.length >= esTamanho) {
					loopping = false;
				}
			}
		}

		if (loopping === true) {
			aloc_sMensagem = Buffer.alloc(esTamanho);
			return POS_UltimoRetorno(aloc_sMensagem, aloc_esTamanho, cb);
		}

		cbx(cb)({ success: true, message: sMensagem, data: false });
	} catch (e) {
		console.log('CheckResult <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Inicializar = function (cb) {
	try {
		console.log('POS_Inicializar >>> ' + JSON.stringify([eArqConfig, eChaveCrypt]));
		ACBrLibPosPrinter.POS_Inicializar.async(eArqConfig, eChaveCrypt, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Inicializar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Finalizar = function (cb) {
	try {
		console.log('POS_Finalizar >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_Finalizar.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Finalizar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_UltimoRetorno = function (aloc_sMensagem, aloc_esTamanho, cb) {
	try {

		console.log('POS_UltimoRetorno >>> ' + JSON.stringify(['aloc_sMensagem', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_UltimoRetorno.async(aloc_sMensagem, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sMensagem, aloc_esTamanho, cb);
		});

	} catch (e) {
		console.log('POS_UltimoRetorno <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Nome = function (cb) {
	try {
		var aloc_sNome = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		console.log('POS_Nome >>> ' + JSON.stringify(['aloc_sNome', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_Nome.async(aloc_sNome, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sNome, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_Nome <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Versao = function (cb) {
	try {
		var aloc_sVersao = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		console.log('POS_Versao >>> ' + JSON.stringify(['aloc_sVersao', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_Versao.async(aloc_sVersao, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sVersao, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_Versao <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ConfigLer = function (cb) {
	try {
		console.log('POS_ConfigLer >>> ' + JSON.stringify([eArqConfig]));
		ACBrLibPosPrinter.POS_ConfigLer.async(eArqConfig, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ConfigLer <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ConfigGravar = function (cb) {
	try {
		console.log('POS_ConfigGravar >>> ' + JSON.stringify([eArqConfig]));
		ACBrLibPosPrinter.POS_ConfigGravar.async(eArqConfig, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ConfigGravar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ConfigLerValor = function (eSessao, eChave, cb) {
	try {
		var aloc_sValor = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		console.log('POS_ConfigLerValor >>> ' + JSON.stringify([eSessao, eChave, 'aloc_sValor', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_ConfigLerValor.async(eSessao, eChave, aloc_sValor, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sValor, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_ConfigLerValor <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ConfigGravarValor = function (eSessao, eChave, sValor, cb) {
	try {
		sValor = Buffer.from(sValor);

		console.log('POS_ConfigGravarValor >>> ' + JSON.stringify([eSessao, eChave, sValor]));
		ACBrLibPosPrinter.POS_ConfigGravarValor.async(eSessao, eChave, sValor, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ConfigGravarValor <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Ativar = function (cb) {
	try {
		console.log('POS_Ativar >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_Ativar.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Ativar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Desativar = function (cb) {
	try {
		console.log('POS_Desativar >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_Desativar.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Desativar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_TxRx = function (eCmd, NytesToRead, ATimeOut, WaitForTerminator, cb) {
	try {
		eCmd = Buffer.from(eCmd);

		var aloc_sResposta = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		WaitForTerminator = (WaitForTerminator ? true : false);

		console.log('POS_TxRx >>> ' + JSON.stringify([eCmd, NytesToRead, ATimeOut, WaitForTerminator, 'aloc_sResposta', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_TxRx.async(eCmd, NytesToRead, ATimeOut, WaitForTerminator, aloc_sResposta, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sResposta, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_TxRx <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Zerar = function (cb) {
	try {
		console.log('POS_Zerar >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_Zerar.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Zerar <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_InicializarPos = function (cb) {
	try {
		console.log('POS_InicializarPos >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_InicializarPos.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_InicializarPos <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Reset = function (cb) {
	try {
		console.log('POS_Reset >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_Reset.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Reset <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_PularLinhas = function (NumLinhas, cb) {
	try {
		console.log('POS_PularLinhas >>> ' + JSON.stringify([NumLinhas]));
		ACBrLibPosPrinter.POS_PularLinhas.async(NumLinhas, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_PularLinhas <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_CortarPapel = function (Parcial, cb) {
	try {
		Parcial = (Parcial ? true : false);

		console.log('POS_CortarPapel >>> ' + JSON.stringify([Parcial]));
		ACBrLibPosPrinter.POS_CortarPapel.async(Parcial, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_CortarPapel <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_AbrirGaveta = function (cb) {
	try {
		console.log('POS_AbrirGaveta >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_AbrirGaveta.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_AbrirGaveta <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_LerInfoImpressora = function (cb) {
	try {
		var aloc_sResposta = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		console.log('POS_LerInfoImpressora >>> ' + JSON.stringify(['aloc_sResposta', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_LerInfoImpressora.async(aloc_sResposta, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sResposta, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_LerInfoImpressora <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_LerStatusImpressora = function (Tentativas, cb) {
	try {
		var aloc_status = ref.alloc('long');

		console.log('POS_LerStatusImpressora >>> ' + JSON.stringify([Tentativas, 'aloc_status']));
		ACBrLibPosPrinter.POS_LerStatusImpressora.async(Tentativas, aloc_status, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			var status = aloc_status.deref();
			var sMensagem = 0;
			console.log('POS_LerStatusImpressora <<< status = ' + JSON.stringify(status));

			switch (status) {
				case 0:
					sMensagem = 'None';
					break;

				case 1 << 0:
					sMensagem = 'Erro';
					break;

				case 1 << 1:
					sMensagem = 'Não Serial';
					break;

				case 1 << 2:
					sMensagem = 'Pouco Papel';
					break;

				case 1 << 3:
					sMensagem = 'Sem Papel';
					break;

				case 1 << 4:
					sMensagem = 'Gaveta Aberta';
					break;

				case 1 << 5:
					sMensagem = 'Imprimindo';
					break;

				case 1 << 6:
					sMensagem = 'Offline';
					break;

				case 1 << 7:
					sMensagem = 'Tampa Aberta';
					break;

				case 1 << 8:
					sMensagem = 'Erro de Leitura';
					break;

				default:
					sMensagem = 'Desconhecido';
					break;
			}

			cbx(cb)({ success: true, message: sMensagem, data: false });
		});
	} catch (e) {
		console.log('POS_LerStatusImpressora <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_RetornarTags = function (cb) {
	try {
		var aloc_sResposta = Buffer.alloc(buflength);
		var aloc_esTamanho = ref.alloc('int', buflength);

		console.log('POS_RetornarTags >>> ' + JSON.stringify(['aloc_sResposta', 'aloc_esTamanho']));
		ACBrLibPosPrinter.POS_RetornarTags.async(1, aloc_sResposta, aloc_esTamanho, function (err, Result) {
			if (err)
				throw err;

			CheckResult(Result, aloc_sResposta, aloc_esTamanho, cb);
		});
	} catch (e) {
		console.log('POS_RetornarTags <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_Imprimir = function (eString, PulaLinha, DecodificarTags, CodificarPagina, Copias, cb) {
	try {
		eString = Buffer.from(eString);

		console.log('POS_Imprimir >>> ' + JSON.stringify([eString, PulaLinha, DecodificarTags, CodificarPagina, Copias]));
		ACBrLibPosPrinter.POS_Imprimir.async(eString, PulaLinha, DecodificarTags, CodificarPagina, Copias, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_Imprimir <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ImprimirLinha = function (eString, cb) {
	try {
		eString = Buffer.from(eString);

		console.log('POS_ImprimirLinha >>> ' + JSON.stringify([eString]));
		ACBrLibPosPrinter.POS_ImprimirLinha.async(eString, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ImprimirLinha <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ImprimirCmd = function (eComando, cb) {
	try {
		eComando = Buffer.from(eComando);

		console.log('POS_ImprimirCmd >>> ' + JSON.stringify([eComando]));
		ACBrLibPosPrinter.POS_ImprimirCmd.async(eComando, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ImprimirCmd <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}

var POS_ImprimirTags = function (cb) {
	try {
		console.log('POS_ImprimirTags >>> ' + JSON.stringify([]));
		ACBrLibPosPrinter.POS_ImprimirTags.async(tvoid, function (err, Result) {
			if (err)
				throw err;

			if (Result !== 0)
				return cbx(cb)({ success: false, message: getResultMessage(Result), data: false });

			cbx(cb)({ success: true, message: 'Ok', data: false });
		});
	} catch (e) {
		console.log('POS_ImprimirTags <<< ' + e.message);
		cbx(cb)({ success: false, message: e.message, data: false });
	}
}






POS_Inicializar(function (resultado) {
	console.log('POS_Inicializar', resultado.message);
	if (resultado.success === true) {

		// POS_Nome(function (resultado) {
		// 	console.log('POS_Nome', resultado.message);
		// 	POS_Finalizar();
		// });

		// POS_Versao(function (resultado) {
		// 	console.log('POS_Versao', resultado.message);
		// 	POS_Finalizar();
		// });

		// var doSetConfig = function (i, cb) {
		// 	if (i < aINI.length) {
		// 		var item = aINI[i]
		// 		POS_ConfigGravarValor(item.section, item.key, item.value, function (resultado) {
		// 			console.log('POS_ConfigGravarValor', resultado.message);
		// 			if (resultado.success === true) {
		// 				doSetConfig(i + 1, cb);
		// 				return;
		// 			}
		// 			cbx(cb)(resultado);
		// 		});
		// 		return;
		// 	}
		// 	cbx(cb)({ success: true, message: 'Salvo com sucesso.', data: false });
		// }

		// doSetConfig(0, function (resultado) {
		// 	POS_Finalizar();
		// });

		// POS_ConfigLerValor('Sistema', 'Versao', function (resultado) {
		// 	console.log('POS_ConfigLerValor', resultado.message);
		// 	POS_Finalizar();
		// });

		POS_Ativar(function (resultado) {
			console.log('POS_Ativar', resultado.message);
			if (resultado.success === true) {

				var doClose = function () {
					POS_Desativar(function (resultado) {
						POS_Finalizar();
					});
				}

				// var eCmd = 'A ACBrLibPosPrinter é uma biblioteca (dll) desenvolvida usando o componente ACBrPosPrinter do Projeto ACBr, o qual permite a comunicação direta com impressoras não fiscais que implementam a linguagem EscPos,permitindo a utilização de todos os recursos disponíveis nestas impressoras, como diferentes fontes e formatações, corte de papel, impressão de QRCode, entre outros.';
				// POS_TxRx(eCmd, 1, 10, '0', function (resultado) {
				// 	console.log('POS_TxRx', resultado.message);
				// 	doClose();
				// });

				// POS_Zerar(function (resultado) {
				// 	console.log('POS_Zerar', resultado.message);
				// 	doClose();
				// });

				// POS_InicializarPos(function (resultado) {
				// 	console.log('POS_InicializarPos', resultado.message);
				// 	doClose();
				// });

				// POS_Reset(function (resultado) {
				// 	console.log('POS_Reset', resultado.message);
				// 	doClose();
				// });

				// POS_PularLinhas(20, function (resultado) {
				// 	console.log('POS_PularLinhas', resultado.message);
				// 	doClose();
				// });

				// POS_CortarPapel(1, function (resultado) {
				// 	console.log('POS_CortarPapel', resultado.message);
				// 	doClose();
				// });

				// POS_AbrirGaveta(function (resultado) {
				// 	console.log('POS_AbrirGaveta', resultado.message);
				// 	doClose();
				// });

				// POS_LerInfoImpressora(function (resultado) {
				// 	console.log('POS_LerInfoImpressora', resultado.message);
				// 	doClose();
				// });

				// POS_LerStatusImpressora(1, function (resultado) {
				// 	console.log('POS_LerStatusImpressora', resultado.message);
				// 	doClose();
				// });

				// POS_RetornarTags(function (resultado) {
				// 	console.log('POS_RetornarTags', resultado.message.split('|').join("\n"));
				// 	doClose();
				// });

				var eCmd = 'A ACBrLibPosPrinter é uma biblioteca (dll) </linha_simples> desenvolvida usando o componente ACBrPosPrinter do Projeto ACBr</linha_simples>, o qual permite a comunicação direta com impressoras não fiscais que implementam a linguagem EscPos,permitindo a utilização de todos os recursos disponíveis nestas impressoras, como diferentes fontes e formatações, corte de papel, impressão de QRCode, entre outros.';
				POS_Imprimir(eCmd, true, true, false, 2, function (resultado) {
					console.log('POS_Imprimir', resultado.message);
					doClose();
				});

				// var eCmd = 'A ACBrLibPosPrinter é uma biblioteca (dll) </linha_simples> desenvolvida usando o componente ACBrPosPrinter do Projeto ACBr</linha_simples>, o qual permite a comunicação direta com impressoras não fiscais que implementam a linguagem EscPos,permitindo a utilização de todos os recursos disponíveis nestas impressoras, como diferentes fontes e formatações, corte de papel, impressão de QRCode, entre outros.';
				// POS_ImprimirLinha(eCmd, function (resultado) {
				// 	console.log('POS_ImprimirLinha', resultado.message);
				// 	doClose();
				// });

				// var eCmd = 'A ACBrLibPosPrinter é uma biblioteca (dll) </linha_simples> desenvolvida usando o componente ACBrPosPrinter do Projeto ACBr</linha_simples>, o qual permite a comunicação direta com impressoras não fiscais que implementam a linguagem EscPos,permitindo a utilização de todos os recursos disponíveis nestas impressoras, como diferentes fontes e formatações, corte de papel, impressão de QRCode, entre outros.';
				// POS_ImprimirCmd(eCmd, function (resultado) {
				// 	console.log('POS_ImprimirCmd', resultado.message);
				// 	doClose();
				// });

				// POS_ImprimirTags(function (resultado) {
				// 	console.log('POS_ImprimirTags', resultado.message);
				// 	doClose();
				// });

				return;
			}
			POS_Finalizar();

		});

		return;
	}
	POS_Finalizar();
});