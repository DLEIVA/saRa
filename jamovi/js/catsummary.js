// Factor storage includes nominal and ordinal columns; offer nominal columns only.
const update = function(ui) {
    const items = ui.nominalSupplier.value() || [];
    const nominal = items.filter(item => item.properties.measureType === 'nominal');
    if (nominal.length !== items.length) ui.nominalSupplier.setValue(nominal);
};
module.exports = {
    view_loaded: update,
    view_updated: update,
    nominalSupplier_changed: update
};
