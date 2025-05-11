Use Banco;

DELIMITER $$

/*
	realizarMovimiento(cuenta, tipo, monto)
		- Realiza un movimiento de dinero en una cuenta.
		- Valida que la cuenta exista y tenga saldo suficiente.
		- Registra el movimiento en la tabla de movimientos.
*/

CREATE PROCEDURE realizarMovimiento (
	IN NumeroDeCuenta_ CHAR(12),
    IN Tipo_ INT,
    IN Monto_ DECIMAL(18,2)
)
BEGIN
	DECLARE SaldoActual DECIMAL (18, 2);
    DECLARE Ci_ VARCHAR(8);
    DECLARE ExisteCuenta INT DEFAULT FALSE;
    -- Valida Tipo de operacion
	IF Tipo_ IN (1, 2) THEN

        SET ExisteCuenta = (SELECT COUNT(*) FROM CUENTAS C WHERE C.NumeroDeCuenta = NumeroDeCuenta_);

		-- Existencia de cuenta
		IF ExisteCuenta THEN

			SELECT C.Saldo, C.Ci
			INTO SaldoActual, Ci_
			FROM CUENTAS C
			WHERE C.NumeroDeCuenta = NumeroDeCuenta_;

            -- Saldo suficiente para la operacion
			IF SaldoActual >= Monto_ THEN

			    CASE
			        WHEN Tipo_ = 1 THEN
			            SET SaldoActual = SaldoActual + Monto_;
			        WHEN Tipo_ = 2 THEN
			            SET SaldoActual = SaldoActual - Monto_;
			            SET Monto_ = Monto_ * -1;
			    END CASE;

                -- Actualiza el saldo
			    UPDATE CUENTAS C
                SET C.Saldo = SaldoActual
                WHERE C.NumeroDeCuenta = NumeroDeCuenta_ AND C.Ci = Ci_;

                -- Registra Movimiento
                INSERT INTO MOVIMIENTOS (
					Ci,
					NumeroDeCuenta,
					Tipo,
					Monto,
					Fecha)
                    VALUES (Ci_, NumeroDeCuenta_, Tipo_, Monto_, NOW());
            ELSE
				SIGNAL SQLSTATE '45000'
                SET MESSAGE_TEXT = 'La cuenta no dispone de fondos suficientes para la operacion.';
            END IF;
        ELSE
			SIGNAL SQLSTATE '45000'
			SET MESSAGE_TEXT = 'La cuenta indicada no existe.';
        END IF;
	ELSE
		SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Tipo de operacion incorrecta, debe ser:\n 1. DEPOSITO\n2. RETIRO';
	END IF;
END$$

/*
	transferenciaEntreCuentas(cuenta_origen, cuenta_destino, monto)
		- Transfiere dinero entre cuentas de la misma moneda.
		- Valida que ambas cuentas existan y estén en la misma moneda.
		- Valida saldo suficiente.
		- Registra los movimientos en ambas cuentas
*/

CREATE PROCEDURE transferenciaEntreCuentas(
	IN CuentaDeOrigen_ CHAR(12),
    IN CuentaDeDestino_ CHAR(12),
    IN Monto_ DECIMAL(18, 2)
)
BEGIN
	DECLARE ExistOrigin, ExistTarget INT DEFAULT FALSE;
    DECLARE MonedaOrigin, MonedaTarget VARCHAR(3);
	DECLARE Ci_Origin, Ci_Target VARCHAR(8);
    DECLARE SaldoOrigin DECIMAL(18, 2);
    DECLARE Ref_ INT;

    SET ExistOrigin = (SELECT COUNT(*) FROM CUENTAS C WHERE C.NumeroDeCuenta = CuentaDeOrigen_);
    SET ExistTarget = (SELECT COUNT(*) FROM CUENTAS C WHERE C.NumeroDeCuenta = CuentaDeDestino_);

    IF (ExistOrigin AND ExistTarget) THEN

		SELECT C.Moneda, C.Ci, C.Saldo INTO MonedaOrigin, Ci_Origin, SaldoOrigin FROM CUENTAS C WHERE C.NumeroDeCuenta = CuentaDeOrigen_;
		SELECT C.Moneda, C.Ci INTO MonedaTarget, Ci_Target FROM CUENTAS C WHERE C.NumeroDeCuenta = CuentaDeDestino_;

		IF MonedaOrigin = MonedaTarget THEN
			IF SaldoOrigin >= Monto_ THEN

			    -- Actualiza el saldo
			    SET SaldoOrigin = SaldoOrigin - Monto_;

                UPDATE CUENTAS C
                SET C.Saldo = SaldoOrigin
                WHERE C.NumeroDeCuenta = CuentaDeOrigen_ AND C.Ci = Ci_Origin;

				-- Registra Movimiento
                INSERT INTO MOVIMIENTOS (
					Ci,
					NumeroDeCuenta,
					Tipo,
					Monto,
					Fecha)
                    VALUES (Ci_Origin, CuentaDeOrigen_, 3, (Monto_ * -1), NOW());

				SET Ref_ = LAST_INSERT_ID();

			    -- Inserta ref en movimiento de debito al siguiente movimiento (credito)
			    UPDATE MOVIMIENTOS
			    SET Ref = LAST_INSERT_ID() + 1
			    WHERE IdMovimiento = Ref_;

				-- Actualiza el saldo
                UPDATE CUENTAS C
                SET C.Saldo = (C.Saldo + Monto_)
                WHERE C.NumeroDeCuenta = CuentaDeDestino_ AND C.Ci = Ci_Target;

				-- Registra Movimiento
                INSERT INTO MOVIMIENTOS (
					Ci,
					NumeroDeCuenta,
					Tipo,
					Monto,
					Fecha,
                    Ref)
                    VALUES (Ci_Target, CuentaDeDestino_, 3, Monto_, NOW(), Ref_);
            ELSE
				SIGNAL SQLSTATE '45000'
				SET MESSAGE_TEXT = 'La cuentas de origen no tiene fondos suficientes para la transferencia.';
            END IF;
        ELSE
			SIGNAL SQLSTATE '45000'
			SET MESSAGE_TEXT = 'Las cuentas de origen y destino deben tener la mimsa moneda.';
        END IF;
    ELSE
		SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Las cuentas de origen y destino deben existir.';
    END IF;

END$$

/*
	calcularSaldosPorClienteMoneda(cliente)
	   - Utiliza un cursor.
	   - Recorre las cuentas del cliente y calcula el saldo total por moneda.
	   - Inserta o actualiza en la tabla de saldos por moneda.
*/

CREATE PROCEDURE calcularSaldosPorClienteMoneda(
	IN Ci_ VARCHAR(8)
)
BEGIN
	DECLARE Moneda_ VARCHAR(3);
	DECLARE SaldoTotal_NEW DECIMAL(18, 2);
	DECLARE FIN INT DEFAULT FALSE;

	DECLARE ACC_CURSOR CURSOR FOR (
    SELECT M.MonedaId, SUM(C.Saldo) AS SaldoTotal FROM CUENTAS C INNER JOIN MONEDAS M ON C.Moneda = M.MonedaId WHERE C.Ci = Ci_ GROUP BY C.Ci, M.MonedaId);
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET FIN = TRUE;

	OPEN ACC_CURSOR;

	FETCH ACC_CURSOR INTO Moneda_, SaldoTotal_NEW;

    WHILE FIN != TRUE DO
		IF ( SELECT COUNT(*) FROM SALDOS_CLIENTE_MONEDA SCM WHERE SCM.Ci = Ci_ AND SCM.MonedaId = Moneda_) = 1 THEN
			UPDATE SALDOS_CLIENTE_MONEDA SCM
			SET SCM.SaldoTotal = SaldoTotal_NEW
			WHERE SCM.Ci = Ci_ AND SCM.MonedaId = Moneda_;
        ELSE
			INSERT INTO SALDOS_CLIENTE_MONEDA VALUES (Ci_, Moneda_, SaldoTotal_NEW);
		END IF;
        FETCH ACC_CURSOR INTO Moneda_, SaldoTotal_NEW;
    END WHILE;

    CLOSE ACC_CURSOR;
END$$

/*
	Trigger de borrado de cuentas
		- Impide borrar cuentas con movimientos asociados.
		- Registra el intento en una tabla de auditoría.
*/

CREATE TRIGGER RegistroAuditoria
    BEFORE DELETE
    ON CUENTAS
    FOR EACH ROW
BEGIN
    IF (SELECT COUNT(*) FROM MOVIMIENTOS M WHERE M.Ci = OLD.Ci AND M.NumeroDeCuenta = OLD.NumeroDeCuenta) > 0 THEN
        BEGIN
            INSERT INTO AUDITORIA(Ci, NumeroDeCuenta, Fecha) VALUES (OLD.Ci, OLD.NumeroDeCuenta, NOW());
            SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'No es posible borrar una cuenta asociada a movientos.';
        END;
    END IF;
END$$

/*
	Trigger sobre actualización de saldo
		- Si el saldo queda en cero, actualiza la fecha de apertura de la cuenta a la fecha actual.
*/

CREATE TRIGGER ActualizacionDeSaldo_Update
BEFORE UPDATE ON CUENTAS
FOR EACH ROW
BEGIN
	IF NEW.Saldo = 0 THEN
        SET NEW.FechaApertura = NOW();
    END IF;
END$$

/*
	Trigger tras inserción de movimiento
		- Recalcula automáticamente los saldos totales por cliente y moneda.
*/

CREATE TRIGGER NuevoMovimiento
AFTER INSERT ON Movimientos
FOR EACH ROW
BEGIN
	CALL calcularSaldosPorClienteMoneda(NEW.Ci);
END$$

/*
	Trigger tras inserción de una cuenta
		- Inserta en la tabla de cuentas por moneda.
		- Recalcula automáticamente los saldos totales por cliente y moneda.
*/

CREATE TRIGGER Cuenta_Clientes_Nueva
AFTER INSERT ON Cuentas
FOR EACH ROW
BEGIN
	INSERT INTO Cuenta_Moneda VALUES(NEW.Moneda, NEW.Ci, NEW.NumeroDeCuenta);
	CALL calcularSaldosPorClienteMoneda(NEW.Ci);
END$$

DELIMITER ;
SHOW TRIGGERS;